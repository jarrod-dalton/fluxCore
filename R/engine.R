#' Engine
#'
#' Orchestrates simulation by repeatedly proposing the next event(s), applying a transition
#' patch, recording the event on a Entity, and stopping when bundle$stop() returns TRUE
#' (or a max_time / max_events limit is reached).
#'
#' @section Run result:
#' `Engine$run()` returns a list containing the updated `entity`, its `events`,
#' optional `observations`, and `stopped_by`. The termination value is one of
#' `"stop"`, `"max_time"`, `"max_events"`, or `"no_proposals"`. When trajectory
#' logging is configured, the result also contains `trajectory_records`.
#'
#' Direct `Engine$run()` calls construct one `ParamContext` from
#' `bundle$params`, or an empty parameter list, unless a Core cohort harness
#' supplies a prevalidated context. `Engine$run_draw()` remains a raw parameter
#' payload entry point. Its caller owns RNG setup, so a RuntimeContext stored on
#' the Engine does not reseed the internal draw run.
#'
#' @export
Engine <- R6::R6Class(
  classname = "Engine",
  public = list(
    bundle = NULL,
    time_spec = NULL,

    # v2.0.0 fields -- set by load_model(); NULL when using Engine$new() directly
    .v2_mode     = FALSE,
    .schema      = NULL,
    .policy      = NULL,
    .environment = NULL,
    .trajectory  = NULL,
    .runtime     = NULL,
    .param_source = NULL,

    initialize = function(bundle = NULL, ...) {
      if (is.null(bundle)) {
        stop(
          "Engine$new() requires a `bundle` argument. ",
          "Supply a ModelBundle list directly, or use load_model() for the full v2 assembly path.",
          call. = FALSE
        )
      }
      .validate_model_bundle(bundle)
      self$bundle <- bundle
      self$time_spec <- bundle$time_spec
      invisible(self)
    },

    # Run a simulation for one entity (see README for high-level flow)
    run = function(entity,
                   max_events = 1000,
                   max_time = NULL,
                   return_observations = TRUE,
                   .internal_ctx = NULL) {

      # .internal_ctx is used only by Core run harnesses to pass run metadata,
      # a prevalidated ParamContext, and private RNG ownership. It is not a
      # user-facing parameter.
      if (is.null(.internal_ctx)) .internal_ctx <- list()

      run_id <- if (!is.null(.internal_ctx$run_id)) as.character(.internal_ctx$run_id) else "run_1"
      entity_id <- if (!is.null(entity$id) && nzchar(as.character(entity$id))) {
        as.character(entity$id)
      } else if (!is.null(.internal_ctx$entity_id) && nzchar(as.character(.internal_ctx$entity_id))) {
        as.character(.internal_ctx$entity_id)
      } else {
        "entity"
      }

      # A cohort supplies one already-normalized ParamContext. Direct run paths
      # continue to supply raw payloads and construct their one context here.
      supplied_param_ctx <- .internal_ctx$param_ctx
      if (!is.null(supplied_param_ctx) && !inherits(supplied_param_ctx, "ParamContext")) {
        stop("Internal error: `.internal_ctx$param_ctx` must be a ParamContext.", call. = FALSE)
      }
      params <- if (is.null(supplied_param_ctx)) {
        if (!is.null(.internal_ctx$params)) {
          .internal_ctx$params
        } else if (!is.null(self$bundle$params) && is.list(self$bundle$params)) {
          self$bundle$params
        } else {
          list()
        }
      } else {
        NULL
      }

      # Stage 3 hardening: apply RuntimeContext seed in single-run v2 path.
      if (isTRUE(self$.v2_mode) &&
          !isTRUE(.internal_ctx$.rng_owned_by_harness) &&
          !is.null(self$.runtime) &&
          !is.null(self$.runtime$seed)) {
        draw_id <- if (!is.null(.internal_ctx$param_draw_id)) as.integer(.internal_ctx$param_draw_id) else 1L
        replicate_id <- if (!is.null(self$.runtime$replicate_id)) as.integer(self$.runtime$replicate_id) else 1L
        local_seed <- .seed_for(as.integer(self$.runtime$seed), entity_id, draw_id, replicate_id)
        set.seed(local_seed)
      }

      resolve_trajectory_config <- function(trajectory) {
        if (is.null(trajectory)) return(NULL)
        if (!is.list(trajectory)) {
          stop("Engine$run(): `.trajectory` must be a list or NULL.", call. = FALSE)
        }

        detail <- trajectory$detail
        if (is.null(detail)) detail <- "none"
        if (!is.character(detail) || length(detail) != 1L || !(detail %in% c("none", "summary", "full"))) {
          stop("Engine$run(): trajectory detail must be one of 'none', 'summary', or 'full'.", call. = FALSE)
        }

        summary_fn <- trajectory$summary_fn
        if (is.null(summary_fn)) summary_fn <- state_summary_default
        if (!is.function(summary_fn)) {
          stop("Engine$run(): trajectory summary_fn must be a function when provided.", call. = FALSE)
        }

        list(detail = detail, summary_fn = summary_fn)
      }

      fired_decision_points <- function(schema, event, v2_mode = FALSE) {
        if (!isTRUE(v2_mode)) return(list())
        if (is.null(schema) || is.null(schema$decision_points) || length(schema$decision_points) == 0L) return(list())
        out <- list()
        for (dp in schema$decision_points) {
          if (dp_fires(dp, event)) out[[length(out) + 1L]] <- dp
        }
        out
      }

      decision_observation <- function(dp, entity) {
        if (!is.null(dp$observation_fn)) {
          out <- dp$observation_fn(entity)
        } else {
          out <- state_summary_default(entity)
        }
        if (!is.list(out)) {
          warning(sprintf("DecisionPoint('%s') observation_fn did not return a list; coercing via as.list().", dp$id),
                  call. = FALSE)
          out <- as.list(out)
        }
        out
      }

      capture_trajectory_state <- function(entity, traj_cfg, when = c("before", "after")) {
        if (is.null(traj_cfg)) return(NULL)
        when <- match.arg(when)
        detail <- traj_cfg$detail
        if (identical(detail, "none")) return(NULL)
        if (identical(detail, "full")) return(as.list(entity$current))

        out <- traj_cfg$summary_fn(entity)
        if (!is.list(out)) {
          stop(sprintf("Trajectory summary_fn must return a list for state_%s.", when), call. = FALSE)
        }
        out
      }

      as_plain_trajectory_record <- function(x) {
        if (!inherits(x, "TrajectoryRecord")) return(x)
        out <- unclass(x)
        if (!is.null(out$selected_action) && inherits(out$selected_action, "ActionEvent")) {
          out$selected_action <- unclass(out$selected_action)
        }
        if (!is.null(out$proposed_actions) && is.list(out$proposed_actions) && length(out$proposed_actions) > 0L) {
          out$proposed_actions <- lapply(out$proposed_actions, function(a) {
            if (inherits(a, "ActionEvent")) unclass(a) else a
          })
        }
        out
      }

      traj_cfg <- resolve_trajectory_config(self$.trajectory)

      sim_ctx <- SimContext(
        run_id = run_id,
        time_spec = self$time_spec,
        model_id = NULL,
        scenario_id = NULL,
        horizon = max_time
      )
      param_ctx <- if (!is.null(supplied_param_ctx)) {
        supplied_param_ctx
      } else {
        ParamContext(
          draw_id = if (!is.null(.internal_ctx$param_draw_id)) .internal_ctx$param_draw_id else 1L,
          params = params,
          provenance = NULL
        )
      }

      # One-time initialization hook (optional).
      .call_init_entity(self$bundle, entity)

      obs_accum <- NULL
      trajectory_accum <- list()
      model_event_catalog <- .validate_bundle_event_set(self$bundle$event_catalog, "event_catalog")

      # Build action-handler lookup from DPs and auto-register action event types.
      # `dp_map` retains each decision point's pending-action policy for merging.
      action_handler_map <- list()
      dp_map <- list()
      if (isTRUE(self$.v2_mode) && !is.null(self$.schema$decision_points)) {
        for (dp in self$.schema$decision_points) {
          dp_map[[dp$id]] <- dp
          if (!is.null(dp$action_handlers)) {
            for (atype in names(dp$action_handlers)) {
              action_handler_map[[paste0(dp$id, "::", atype)]] <- dp$action_handlers[[atype]]
              # Auto-register action event types into the engine's catalog
              if (!(atype %in% model_event_catalog)) {
                model_event_catalog <- c(model_event_catalog, atype)
              }
            }
          }
        }
      }

      proposals <- .call_propose_events(self$bundle, entity, sim_ctx = sim_ctx, param_ctx = param_ctx)

      # Engine-owned store of pending policy actions, keyed by decision point id.
      # This is deliberately separate from `proposals`: `proposals` is owned by the
      # model and rewritten by refresh_rules()/propose_events(), whereas pending
      # actions are owned by the engine, survive every refresh, and are retired by
      # the engine itself once realized. Each decision point holds at most one.
      action_proposals <- list()

      # Records why the run ended: "stop", "max_time", "max_events", or
      # "no_proposals". Without this a runaway model is indistinguishable from a
      # normal completion.
      stop_reason <- NULL

      step_once <- function() {
        ev <- .pick_next_event(proposals, action_proposals, event_catalog = model_event_catalog)
        ev_source <- attr(ev, "flux_source")
        ev_key <- attr(ev, "flux_key")

        # Engine-owned action events dispatch to their DecisionPoint's action_handler
        # (when one is registered) instead of bundle$transition().
        action_handler <- NULL
        if (identical(ev_source, "action")) {
          action_handler <- action_handler_map[[paste0(ev_key, "::", ev$event_type)]]
        }

        fired_dps <- fired_decision_points(self$.schema, ev, self$.v2_mode)
        state_before <- if (length(fired_dps) > 0L) capture_trajectory_state(entity, traj_cfg, when = "before") else NULL

        if (!is.null(action_handler)) {
          changes <- .call_action_handler(action_handler, entity, ev, param_ctx = param_ctx)
        } else {
          changes <- .call_transition(self$bundle, entity, ev, sim_ctx = sim_ctx, param_ctx = param_ctx)
        }

        entity$update(time = ev$time_next, event_type = ev$event_type, changes = changes)

        state_after <- if (length(fired_dps) > 0L) capture_trajectory_state(entity, traj_cfg, when = "after") else NULL

        # Evaluate condition (post-transition) for each fired DP.
        # active_dps: condition met (or absent) -> policy is consulted.
        # vetoed_dps: condition false AND audit=TRUE -> audit record only.
        active_dps <- list()
        vetoed_dps <- list()
        for (dp in fired_dps) {
          cond_met <- .evaluate_decision_condition(dp, entity)
          if (cond_met) {
            active_dps[[length(active_dps) + 1L]] <- dp
          } else if (isTRUE(dp$audit)) {
            vetoed_dps[[length(vetoed_dps) + 1L]] <- dp
          }
        }

        # Stage 2B: policy dispatch at declared decision points.
        # Only active in v2 mode with a policy and active decision points.
        action_props <- list()
        selected_actions <- list()
        if (isTRUE(self$.v2_mode) && !is.null(self$.policy) && length(active_dps) > 0L) {
          for (dp in active_dps) {
            proposed_action <- .call_policy(self$.policy, dp, entity, sim_ctx = sim_ctx, param_ctx = param_ctx)
            if (!is.null(proposed_action)) {
              if (!inherits(proposed_action, "ActionEvent")) {
                warning(
                  "policy$propose_action() returned a non-ActionEvent object; ignoring.",
                  call. = FALSE
                )
              } else {
                if (!is.null(dp$allowed_actions) && !(proposed_action$action_type %in% dp$allowed_actions)) {
                  warning(
                    sprintf(
                      "policy returned action_type '%s' not in DecisionPoint('%s') allowed_actions; ignoring.",
                      proposed_action$action_type,
                      dp$id
                    ),
                    call. = FALSE
                  )
                  next
                }
                # Normalize ActionEvent into an event proposal shape expected by
                # arbitration: event_type drives validation/arbitration, while
                # action_type remains for policy provenance.
                if (is.null(proposed_action$event_type) || !nzchar(proposed_action$event_type)) {
                  proposed_action$event_type <- proposed_action$action_type
                }
                # Stage the action by decision point. The merge below applies that
                # decision point's declared policy if an earlier action is pending.
                action_props[[dp$id]] <- proposed_action
                selected_actions[[dp$id]] <- proposed_action
              }
            }
          }
        }

        # Stage 3: emit trajectory records at declared decision points when configured.
        if (!is.null(traj_cfg)) {
          # Active DPs: condition was met (or absent); record with condition_met = TRUE.
          for (dp in active_dps) {
            observation <- decision_observation(dp, entity)
            tr <- TrajectoryRecord(
              run_id = run_id,
              entity_id = entity_id,
              t = entity$last_time,
              decision_point_id = dp$id,
              observation = observation,
              realized_event = ev,
              candidate_actions = dp$allowed_actions,
              proposed_actions = if (!is.null(selected_actions[[dp$id]])) list(selected_actions[[dp$id]]) else list(),
              selected_action = selected_actions[[dp$id]],
              state_before = state_before,
              state_after = state_after,
              condition_met = if (is.null(dp$condition)) NULL else TRUE,
              reward = NULL
            )
            trajectory_accum[[length(trajectory_accum) + 1L]] <<- as_plain_trajectory_record(tr)
          }
          # Vetoed DPs: condition was FALSE and audit=TRUE; no policy call, no action.
          for (dp in vetoed_dps) {
            observation <- decision_observation(dp, entity)
            tr <- TrajectoryRecord(
              run_id = run_id,
              entity_id = entity_id,
              t = entity$last_time,
              decision_point_id = dp$id,
              observation = observation,
              realized_event = ev,
              candidate_actions = dp$allowed_actions,
              proposed_actions = list(),
              selected_action = NULL,
              state_before = state_before,
              state_after = state_after,
              condition_met = FALSE,
              reward = NULL
            )
            trajectory_accum[[length(trajectory_accum) + 1L]] <<- as_plain_trajectory_record(tr)
          }
        }

        if (isTRUE(return_observations)) {
          o <- .call_observe(self$bundle, entity, ev)
          if (!is.null(o)) {
            obs_accum <<- if (is.null(obs_accum)) o else rbind(obs_accum, o)
          }
        }

        if (.call_stop(self$bundle, entity, ev, sim_ctx = sim_ctx, param_ctx = param_ctx)) {
          stop_reason <<- "stop"
          return(FALSE)
        }
        if (!is.null(max_time) && entity$last_time >= max_time) {
          stop_reason <<- "max_time"
          return(FALSE)
        }

        # Retire the realized action. The engine owns this store, so the model is
        # never responsible for clearing it -- and cannot, since it does not name it.
        if (identical(ev_source, "action")) {
          action_proposals[[ev_key]] <<- NULL
        }

        refresh_ids <- .call_refresh_rules(self$bundle, entity, ev, changes)

        if (identical(refresh_ids, "ALL")) {
          proposals <<- .call_propose_events(
            self$bundle, entity,
            sim_ctx = sim_ctx,
            param_ctx = param_ctx,
            last_event = ev
          )
        } else if (length(refresh_ids) > 0) {
          new_props <- .call_propose_events(
            self$bundle, entity,
            process_ids = refresh_ids,
            current_proposals = proposals,
            sim_ctx = sim_ctx,
            param_ctx = param_ctx,
            last_event = ev
          )
          for (pid in refresh_ids) {
            if (!is.null(new_props[[pid]])) {
              proposals[[pid]] <<- new_props[[pid]]
            } else {
              proposals[[pid]] <<- NULL
            }
          }
        }

        # Newly proposed actions join the engine-owned store. Refresh above rewrote
        # only `proposals`, so any action still pending from an earlier step survives.
        # A decision point holds at most one pending action; how a new proposal
        # interacts with an existing one is declared by on_pending_action. Note the
        # realized action was retired above, so a decision point re-proposing right
        # after its own action fired is not a conflict.
        for (dp_id in names(action_props)) {
          pending <- action_proposals[[dp_id]]
          if (is.null(pending)) {
            action_proposals[[dp_id]] <<- action_props[[dp_id]]
            next
          }

          mode <- dp_map[[dp_id]]$on_pending_action
          if (is.null(mode)) mode <- "warn"

          if (identical(mode, "keep")) next
          if (identical(mode, "error")) {
            stop(
              sprintf(
                "DecisionPoint('%s') proposed an action for t=%g while its previous action for t=%g was still pending. Set on_pending_action to allow this.",
                dp_id, action_props[[dp_id]]$time_next, pending$time_next
              ),
              call. = FALSE
            )
          }
          if (identical(mode, "warn")) {
            warning(
              sprintf(
                "DecisionPoint('%s') replaced a still-pending action for t=%g with a new one for t=%g. Set on_pending_action = 'replace' to declare this intentional, or 'keep' to preserve the pending action.",
                dp_id, pending$time_next, action_props[[dp_id]]$time_next
              ),
              call. = FALSE
            )
          }
          action_proposals[[dp_id]] <<- action_props[[dp_id]]
        }

        TRUE
      }

      proposal_count <- function(x) {
        if (length(x) == 0L) return(0L)
        sum(!vapply(x, is.null, logical(1)))
      }

      n <- 0L
      if (proposal_count(proposals) + proposal_count(action_proposals) == 0L) {
        stop_reason <- "no_proposals"
      }
      while (is.null(stop_reason) && n < max_events) {
        n <- n + 1L
        cont <- step_once()
        if (!isTRUE(cont)) break
        if (proposal_count(proposals) + proposal_count(action_proposals) == 0L) {
          stop_reason <- "no_proposals"
          break
        }
      }
      # No reason recorded means the loop ran out of its event budget.
      if (is.null(stop_reason)) stop_reason <- "max_events"

      out <- list(
        entity = entity,
        events = entity$events,
        observations = if (isTRUE(return_observations)) obs_accum else NULL,
        stopped_by = stop_reason
      )
      if (!is.null(traj_cfg)) out$trajectory_records <- trajectory_accum
      out
    },

    # Run a single entity with explicit parameter injection.
    #
    # Public entry point for downstream packages (e.g., fluxForecast streaming
    # functions) that need per-run parameter control without coupling to the
    # internal .internal_ctx structure used by run_cohort(). The caller owns
    # RNG setup for this lower-level harness; a stored Engine runtime does not
    # reseed the internal run.
    #
    # @param entity An Entity object to simulate.
    # @param params Named list of parameter values for this run.
    # @param draw_id Integer identifying the parameter draw (default 1L).
    # @param sim_id Integer identifying the stochastic replicate (default 1L).
    # @param max_events Maximum number of events before stopping.
    # @param max_time Maximum simulation time.
    # @param return_observations Whether to return observations.
    # @return Same structure as Engine$run().
    run_draw = function(entity,
                        params = list(),
                        draw_id = 1L,
                        sim_id = 1L,
                        max_events = 1000,
                        max_time = NULL,
                        return_observations = TRUE) {
      entity_id <- if (!is.null(entity$id) && nzchar(as.character(entity$id))) {
        as.character(entity$id)
      } else {
        "entity"
      }
      run_meta <- list(
        time_spec     = self$time_spec,
        entity_id     = entity_id,
        param_draw_id = as.integer(draw_id),
        sim_id        = as.integer(sim_id),
        params        = params,
        .rng_owned_by_harness = TRUE
      )
      self$run(
        entity = entity,
        max_events = max_events,
        max_time = max_time,
        return_observations = return_observations,
        .internal_ctx = run_meta
      )
    }
  )
)

# v2.0.0: hard error if any bundle callback declares `ctx` as a formal.
.reject_ctx_formal <- function(f, fn_name) {
  if ("ctx" %in% names(formals(f))) {
    stop(
      sprintf("bundle$%s() declares `ctx` as a formal parameter. ", fn_name),
      "`ctx` is removed in fluxCore v2.0.0. ",
      "Declare one model clock in `schema$time_spec` and `bundle$time_spec`; ",
      "callbacks that accept `sim_ctx` can read it from `sim_ctx$time_spec`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Process ids beginning with "." are reserved for engine-internal use. Models must
# not create or address them: the engine owns its own proposal bookkeeping and
# retires it without model involvement.
.reject_reserved_process_ids <- function(ids, source) {
  reserved <- ids[!is.na(ids) & startsWith(ids, ".")]
  if (length(reserved) > 0L) {
    stop(
      sprintf(
        "bundle$%s returned reserved process_id value(s): %s. Process ids beginning with '.' are reserved for fluxCore internal use; choose a name that does not start with a dot.",
        source,
        paste(unique(reserved), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

.call_init_entity <- function(bundle, entity) {
  f <- bundle$init_entity
  if (is.null(f)) return(invisible(NULL))
  if (!is.function(f)) stop("init_entity must be a function if provided.", call. = FALSE)
  .reject_ctx_formal(f, "init_entity")
  args <- list(entity = entity)
  invisible(do.call(f, args))
}

.call_propose_events <- function(bundle, entity, process_ids = NULL, current_proposals = NULL, sim_ctx = NULL, param_ctx = NULL, last_event = NULL) {
  if (is.null(bundle$propose_events) || !is.function(bundle$propose_events)) {
    stop("ModelBundle must provide propose_events(entity, ...).")
  }
  .reject_ctx_formal(bundle$propose_events, "propose_events")

  fml <- names(formals(bundle$propose_events))
  args <- list(entity = entity)
  if ("process_ids" %in% fml) args$process_ids <- process_ids
  if ("current_proposals" %in% fml) args$current_proposals <- current_proposals
  if ("sim_ctx" %in% fml) args$sim_ctx <- sim_ctx
  if ("param_ctx" %in% fml) args$param_ctx <- param_ctx
  # `last_event` is injected only when declared, so bundles written before this
  # argument existed are called exactly as they were. NULL on the initial call,
  # the realized event on every post-event refresh.
  if ("last_event" %in% fml) args$last_event <- last_event

  out <- do.call(bundle$propose_events, args)
  if (is.null(out)) return(list())
  if (!is.list(out)) stop("bundle$propose_events must return a list of events keyed by process_id.")
  if (length(out) == 0L) return(out)

  pids <- names(out)
  if (is.null(pids)) {
    stop("bundle$propose_events must return a *named* list keyed by process_id.", call. = FALSE)
  }
  bad <- which(is.na(pids) | !nzchar(pids))
  if (length(bad) > 0L) {
    stop("bundle$propose_events returned empty or missing process_id names.", call. = FALSE)
  }
  .reject_reserved_process_ids(pids, "propose_events")
  dup <- unique(pids[duplicated(pids)])
  if (length(dup) > 0L) {
    stop(
      sprintf(
        "bundle$propose_events returned duplicated process_id names: %s",
        paste(dup, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  out
}

# Stage 2B: internal policy dispatch helper.
# Accepts either a bare function or a list/environment with a $propose_action method.
# Signature: propose_action(decision_point, entity, ...) -> ActionEvent or NULL
.call_policy <- function(policy, dp, entity, sim_ctx = NULL, param_ctx = NULL) {
  if (is.function(policy)) {
    f <- policy
  } else if (is.list(policy) && is.function(policy$propose_action)) {
    f <- policy$propose_action
  } else {
    warning("policy must be a function or a list with a $propose_action function; ignoring.", call. = FALSE)
    return(NULL)
  }

  fml <- names(formals(f))
  args <- list(decision_point = dp, entity = entity)
  if ("sim_ctx" %in% fml) args$sim_ctx <- sim_ctx
  if ("param_ctx" %in% fml) args$param_ctx <- param_ctx
  result <- tryCatch(
    do.call(f, args),
    error = function(e) {
      stop(
        sprintf(
          "policy$propose_action() errored for DecisionPoint('%s'): %s",
          dp$id,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  .normalize_action_provenance(
    result,
    decision_point_id = dp$id,
    source = "policy$propose_action()"
  )
}

# Evaluate a DecisionPoint condition without assigning callback failures a
# valid model meaning. This helper is shared by every decision-dispatch path so
# ordinary and grouped decisions enforce the same strict scalar contract.
.evaluate_decision_condition <- function(dp, entity) {
  if (is.null(dp$condition)) return(TRUE)

  result <- tryCatch(
    dp$condition(entity),
    error = function(e) {
      stop(
        sprintf(
          "DecisionPoint('%s') condition callback errored: %s",
          dp$id,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (!is.logical(result) || length(result) != 1L || is.na(result)) {
    stop(
      sprintf(
        "DecisionPoint('%s') condition callback must return exactly one non-missing logical value.",
        dp$id
      ),
      call. = FALSE
    )
  }

  result
}

# Normalize the provenance carried by an ActionEvent selected for a particular
# DecisionPoint. The DecisionPoint id owns the pending slot and handler lookup;
# the event may omit that id, but it may not contradict it.
.normalize_action_provenance <- function(action, decision_point_id, source) {
  if (!inherits(action, "ActionEvent")) return(action)

  if (is.null(action$decision_point_id)) {
    action$decision_point_id <- decision_point_id
  } else if (!identical(action$decision_point_id, decision_point_id)) {
    stop(
      sprintf(
        "%s returned an ActionEvent with decision_point_id '%s' while the firing DecisionPoint id is '%s'.",
        source,
        action$decision_point_id,
        decision_point_id
      ),
      call. = FALSE
    )
  }
  action
}

# Dispatch an action_handler from a DecisionPoint.
# Signature: handler(entity, event) -> named list of state updates or NULL
# Optionally accepts param_ctx (auto-detected from formals).
.call_action_handler <- function(handler, entity, event, param_ctx = NULL) {
  fml <- names(formals(handler))
  args <- list(entity = entity, event = event)
  if ("param_ctx" %in% fml) args$param_ctx <- param_ctx
  tryCatch(
    do.call(handler, args),
    error = function(e) {
      action_type <- if (!is.null(event$action_type)) event$action_type else event$event_type
      stop(
        sprintf(
          "action_handler for action_type '%s' from DecisionPoint('%s') errored: %s",
          action_type,
          event$decision_point_id,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

.pick_next_event <- function(proposals, action_proposals = list(), event_catalog = NULL) {
  if (length(proposals) == 0L && length(action_proposals) == 0L) stop("No proposals available.")

  if (length(proposals) > 0L && is.null(names(proposals))) {
    stop("Internal error: proposals must be a named list keyed by process_id.", call. = FALSE)
  }
  keep <- !vapply(proposals, is.null, logical(1))
  proposals <- proposals[keep]

  if (length(action_proposals) > 0L) {
    akeep <- !vapply(action_proposals, is.null, logical(1))
    action_proposals <- action_proposals[akeep]
  }
  if (length(proposals) == 0L && length(action_proposals) == 0L) stop("No proposals available.")

  .validate_event <- function(x, label) {
    if (is.null(x)) return(invisible(FALSE))
    if (!is.list(x)) stop(sprintf("Event proposal for %s must be a list.", label))
    if (is.null(x$time_next) || !is.numeric(x$time_next) || length(x$time_next) != 1L || !is.finite(x$time_next)) {
      stop(sprintf("Event proposal for %s must include numeric scalar time_next.", label))
    }
    if (is.null(x$event_type) || !is.character(x$event_type) || length(x$event_type) != 1L || !nzchar(x$event_type)) {
      stop(sprintf("Event proposal for %s must include character scalar event_type.", label))
    }
    if (!is.null(event_catalog) && !(x$event_type %in% event_catalog)) {
      stop(
        sprintf(
          "Event proposal for %s has event_type '%s' not declared in bundle$event_catalog.",
          label, x$event_type
        ),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }

  pids <- names(proposals)
  for (k in seq_along(proposals)) {
    .validate_event(proposals[[k]], sprintf("process_id '%s'", pids[[k]]))
  }
  akeys <- names(action_proposals)
  for (k in seq_along(action_proposals)) {
    # Actions have no process_id, so identify them by their decision point.
    .validate_event(action_proposals[[k]], sprintf("the action from DecisionPoint('%s')", akeys[[k]]))
  }

  # Arbitrate across both stores at once. Model proposals and engine-owned action
  # proposals compete on equal terms; the deterministic tie-break (time, then id)
  # is unchanged.
  candidates <- c(proposals, action_proposals)
  ids <- c(pids, akeys)
  sources <- c(rep("model", length(proposals)), rep("action", length(action_proposals)))

  times <- vapply(candidates, function(x) x$time_next, numeric(1))
  o <- order(times, ids) # deterministic tie-break: time, then store key
  i <- o[[1]]

  ev <- candidates[[i]]
  # `process_id` is a model-process concept. A realized action is not a model
  # process -- it is identified by its `decision_point_id`, which ActionEvent()
  # already carries. Leaving `process_id` unset keeps the two namespaces from
  # colliding, and makes `is.null(event$process_id)` a reliable marker that the
  # event came from a policy rather than from a model process.
  if (identical(sources[[i]], "model")) {
    ev$process_id <- ids[[i]]
  } else {
    ev$process_id <- NULL
  }
  attr(ev, "flux_source") <- sources[[i]]
  attr(ev, "flux_key") <- ids[[i]]
  ev
}


.call_transition <- function(bundle, entity, ev, sim_ctx = NULL, param_ctx = NULL) {
  f <- bundle$transition
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide transition().")
  .reject_ctx_formal(f, "transition")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("transition() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("sim_ctx" %in% fml) args$sim_ctx <- sim_ctx
  if ("param_ctx" %in% fml) args$param_ctx <- param_ctx
  do.call(f, args)
}


.call_stop <- function(bundle, entity, ev, sim_ctx = NULL, param_ctx = NULL) {
  f <- bundle$stop
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide stop().")
  .reject_ctx_formal(f, "stop")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("stop() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("sim_ctx" %in% fml) args$sim_ctx <- sim_ctx
  if ("param_ctx" %in% fml) args$param_ctx <- param_ctx
  isTRUE(do.call(f, args))
}


.call_observe <- function(bundle, entity, ev) {
  f <- bundle$observe
  if (is.null(f) || !is.function(f)) return(NULL)
  .reject_ctx_formal(f, "observe")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("observe() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  do.call(f, args)
}

.call_refresh_rules <- function(bundle, entity, ev, changes) {
  f <- bundle$refresh_rules
  if (is.null(f) || !is.function(f)) return("ALL")
  .reject_ctx_formal(f, "refresh_rules")
  fml <- names(formals(f))
  args <- list(entity = entity, last_event = ev, changes = changes)
  out <- do.call(f, args)
  if (!is.character(out)) {
    stop(
      "bundle$refresh_rules must return exactly \"ALL\" or a character vector of process_id values.",
      call. = FALSE
    )
  }
  if (length(out) == 1L && identical(out[[1L]], "ALL")) return("ALL")
  if (any(out == "ALL")) {
    stop(
      "bundle$refresh_rules may return \"ALL\" only as a single scalar value.",
      call. = FALSE
    )
  }
  bad <- which(is.na(out) | !nzchar(out))
  if (length(bad) > 0L) {
    stop(
      "bundle$refresh_rules returned empty or missing process_id values.",
      call. = FALSE
    )
  }
  .reject_reserved_process_ids(out, "refresh_rules")
  dup <- unique(out[duplicated(out)])
  if (length(dup) > 0L) {
    stop(
      sprintf(
        "bundle$refresh_rules returned duplicated process_id values: %s",
        paste(dup, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  out
}

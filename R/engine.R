#' Engine
#'
#' Orchestrates simulation by repeatedly proposing the next event(s), applying a transition
#' patch, recording the event on a Entity, and stopping when bundle$stop() returns TRUE
#' (or a max_time / max_events limit is reached).
#'
#' @export
Engine <- R6::R6Class(
  classname = "Engine",
  public = list(
    provider = NULL,
    model_spec = NULL,
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
    .time_spec   = NULL,

    initialize = function(bundle = NULL,
                          provider = NULL,
                          model_spec = list(name = "default"),
                          ...) {
      if (!is.null(bundle)) {
        if (!is.null(provider)) {
          stop("Engine$new(): supply either `bundle` or `provider`, not both.", call. = FALSE)
        }
        .validate_model_bundle(bundle)
        self$provider <- NULL
        self$model_spec <- NULL
        self$bundle <- bundle
        self$time_spec <- bundle$time_spec
        return(invisible(self))
      }

      if (is.null(provider)) provider <- PackageProvider$new()

      self$provider <- provider
      self$model_spec <- model_spec

      if (!is.list(provider) && is.null(provider$load)) {
        stop("provider must be an object with a $load(model_spec, ...) method.")
      }
      if (!is.function(provider$load)) {
        stop("provider$load must be a function.")
      }

      self$bundle <- provider$load(model_spec = model_spec, ...)
      .validate_model_bundle(self$bundle)
      self$time_spec <- self$bundle$time_spec

      invisible(self)
    },

    # Run a simulation for one entity (see README for high-level flow)
    run = function(entity,
                   max_events = 1000,
                   max_time = NULL,
                   return_observations = TRUE,
                   ctx = NULL) {

      # v2.0.0 hard error: Engines assembled via load_model() do not accept ctx.
      # Use SimContext / ParamContext / RuntimeContext instead.
      if (isTRUE(self$.v2_mode) && !is.null(ctx)) {
        stop(
          "Engine$run(): `ctx` is not accepted in v2.0.0 mode (engine was built via load_model()). ",
          "Pass reproducibility settings via RuntimeContext and parameter values via ParamContext.",
          call. = FALSE
        )
      }

      if (is.null(ctx)) ctx <- list()
      if (!is.list(ctx)) stop("ctx must be a list (or NULL).", call. = FALSE)
      .assert_ctx_time_compatible(ctx = ctx, canonical_time_spec = self$time_spec, where = "Engine$run() ctx")
      ctx$time <- .time_ctx_from_spec(self$time_spec)
      ctx$time_spec <- self$time_spec

      # Standardize model parameters in ctx$params.
      # - Users may provide ctx$params to override defaults for a run.
      # - Model bundles may provide bundle$params as a default.
      if (is.null(ctx$params)) {
        if (!is.null(self$bundle$params)) {
          if (!is.list(self$bundle$params)) stop("bundle$params must be a list if provided.", call. = FALSE)
          ctx$params <- self$bundle$params
        } else {
          ctx$params <- list()
        }
      } else {
        if (!is.list(ctx$params)) stop("ctx$params must be a list if provided.", call. = FALSE)
      }

      run_id <- if (!is.null(ctx$run_id)) as.character(ctx$run_id) else "run_1"
      entity_id <- if (!is.null(entity$id) && nzchar(as.character(entity$id))) {
        as.character(entity$id)
      } else if (!is.null(ctx$entity_id) && nzchar(as.character(ctx$entity_id))) {
        as.character(ctx$entity_id)
      } else {
        "entity"
      }

      # Stage 3 hardening: apply RuntimeContext seed in single-run v2 path.
      # Contract: fixed seed + draw_id + replicate_id + entity_id => reproducible output.
      if (isTRUE(self$.v2_mode) && !is.null(self$.runtime) && !is.null(self$.runtime$seed)) {
        draw_id <- if (!is.null(ctx$param_draw_id)) as.integer(ctx$param_draw_id) else 1L
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

      sim_ctx <- NULL
      param_ctx <- NULL
      if (isTRUE(self$.v2_mode)) {
        sim_ctx <- SimContext(
          run_id = run_id,
          time_spec = self$time_spec,
          model_id = NULL,
          scenario_id = NULL,
          horizon = max_time
        )
        param_ctx <- ParamContext(
          draw_id = if (!is.null(ctx$param_draw_id)) ctx$param_draw_id else 1L,
          params = if (!is.null(ctx$params)) ctx$params else list(),
          provenance = NULL
        )
      }

      # One-time initialization hook (optional).
      # Models can use this to register derived variables and perform setup.
      .call_init_entity(self$bundle, entity, ctx = ctx)

      obs_accum <- NULL
      trajectory_accum <- list()
      model_event_catalog <- .validate_bundle_event_set(self$bundle$event_catalog, "event_catalog")

      proposals <- .call_propose_events(self$bundle, entity, ctx = ctx)

      step_once <- function() {
        ev <- .pick_next_event(proposals, event_catalog = model_event_catalog)

        fired_dps <- fired_decision_points(self$.schema, ev, self$.v2_mode)
        state_before <- if (length(fired_dps) > 0L) capture_trajectory_state(entity, traj_cfg, when = "before") else NULL

        changes <- .call_transition(self$bundle, entity, ev, ctx = ctx)

        entity$update(time = ev$time_next, event_type = ev$event_type, changes = changes)

        state_after <- if (length(fired_dps) > 0L) capture_trajectory_state(entity, traj_cfg, when = "after") else NULL

        # Stage 2B: policy dispatch at declared decision points.
        # Only active in v2 mode with a policy and schema decision_points.
        action_props <- list()
        selected_actions <- list()
        if (isTRUE(self$.v2_mode) && !is.null(self$.policy) && length(fired_dps) > 0L) {
          for (dp in fired_dps) {
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
                # Insert as a candidate event proposal under a synthetic process id.
                pid <- paste0(".action.", dp$id)
                action_props[[pid]] <- proposed_action
                selected_actions[[dp$id]] <- proposed_action
              }
            }
          }
        }

        # Stage 3: emit trajectory records at declared decision points when configured.
        if (!is.null(traj_cfg) && length(fired_dps) > 0L) {
          for (dp in fired_dps) {
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
              reward = NULL
            )
            trajectory_accum[[length(trajectory_accum) + 1L]] <<- as_plain_trajectory_record(tr)
          }
        }

        if (isTRUE(return_observations)) {
          o <- .call_observe(self$bundle, entity, ev, ctx = ctx)
          if (!is.null(o)) {
            obs_accum <<- if (is.null(obs_accum)) o else rbind(obs_accum, o)
          }
        }

        if (.call_stop(self$bundle, entity, ev, ctx = ctx)) return(FALSE)
        if (!is.null(max_time) && entity$last_time >= max_time) return(FALSE)

        refresh_ids <- .call_refresh_rules(self$bundle, entity, ev, changes, ctx = ctx)

        if (identical(refresh_ids, "ALL")) {
          proposals <<- .call_propose_events(self$bundle, entity, ctx = ctx)
        } else if (length(refresh_ids) > 0) {
          new_props <- .call_propose_events(
            self$bundle, entity, ctx = ctx,
            process_ids = refresh_ids,
            current_proposals = proposals
          )
          for (pid in refresh_ids) {
            if (!is.null(new_props[[pid]])) {
              proposals[[pid]] <<- new_props[[pid]]
            } else {
              proposals[[pid]] <<- NULL
            }
          }
        }

        if (length(action_props) > 0L) {
          proposals <<- utils::modifyList(proposals, action_props, keep.null = TRUE)
        }

        TRUE
      }

      n <- 0L
      while (n < max_events) {
        n <- n + 1L
        cont <- step_once()
        if (!isTRUE(cont)) break
        if (length(proposals) == 0L) break
      }

      out <- list(
        entity = entity,
        events = entity$events,
        observations = if (isTRUE(return_observations)) obs_accum else NULL
      )
      if (!is.null(traj_cfg)) out$trajectory_records <- trajectory_accum
      out
    }
  )
)

.call_init_entity <- function(bundle, entity, ctx = NULL) {
  f <- bundle$init_entity
  if (is.null(f)) return(invisible(NULL))
  if (!is.function(f)) stop("init_entity must be a function if provided.", call. = FALSE)

  fml <- names(formals(f))
  args <- list(entity = entity)
  if ("ctx" %in% fml) args$ctx <- ctx
  invisible(do.call(f, args))
}

.call_propose_events <- function(bundle, entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
  if (is.null(bundle$propose_events) || !is.function(bundle$propose_events)) {
    stop("ModelBundle must provide propose_events(entity, ctx, ...).")
  }

  fml <- names(formals(bundle$propose_events))
  args <- list(entity = entity)
  if ("ctx" %in% fml) args$ctx <- ctx
  if ("process_ids" %in% fml) args$process_ids <- process_ids
  if ("current_proposals" %in% fml) args$current_proposals <- current_proposals

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
  tryCatch(
    do.call(f, args),
    error = function(e) {
      warning(sprintf("policy$propose_action() errored for dp '%s': %s", dp$id, conditionMessage(e)),
              call. = FALSE)
      NULL
    }
  )
}

.pick_next_event <- function(proposals, event_catalog = NULL) {
  if (length(proposals) == 0L) stop("No proposals available.")

  if (is.null(names(proposals))) {
    stop("Internal error: proposals must be a named list keyed by process_id.", call. = FALSE)
  }
  keep <- !vapply(proposals, is.null, logical(1))
  proposals <- proposals[keep]
  if (length(proposals) == 0L) stop("No proposals available.")

  .validate_event <- function(x, pid) {
    if (is.null(x)) return(invisible(FALSE))
    if (!is.list(x)) stop(sprintf("Event proposal for process_id '%s' must be a list.", pid))
    if (is.null(x$time_next) || !is.numeric(x$time_next) || length(x$time_next) != 1L || !is.finite(x$time_next)) {
      stop(sprintf("Event proposal for process_id '%s' must include numeric scalar time_next.", pid))
    }
    if (is.null(x$event_type) || !is.character(x$event_type) || length(x$event_type) != 1L || !nzchar(x$event_type)) {
      stop(sprintf("Event proposal for process_id '%s' must include character scalar event_type.", pid))
    }
    if (!is.null(event_catalog) && !(x$event_type %in% event_catalog)) {
      stop(
        sprintf(
          "Event proposal for process_id '%s' has event_type '%s' not declared in bundle$event_catalog.",
          pid, x$event_type
        ),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }

  pids <- names(proposals)
  for (k in seq_along(proposals)) .validate_event(proposals[[k]], pids[[k]])

  times <- vapply(proposals, function(x) x$time_next, numeric(1))
  o <- order(times, pids) # deterministic tie-break: time, then process_id
  pid <- pids[[o[[1]]]]
  ev <- proposals[[pid]]
  ev$process_id <- pid
  ev
}


.call_transition <- function(bundle, entity, ev, ctx = NULL) {
  f <- bundle$transition
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide transition().")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("transition() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  do.call(f, args)
}


.call_stop <- function(bundle, entity, ev, ctx = NULL) {
  f <- bundle$stop
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide stop().")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("stop() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  isTRUE(do.call(f, args))
}


.call_observe <- function(bundle, entity, ev, ctx = NULL) {
  f <- bundle$observe
  if (is.null(f) || !is.function(f)) return(NULL)
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("observe() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  do.call(f, args)
}

.call_refresh_rules <- function(bundle, entity, ev, changes, ctx = NULL) {
  f <- bundle$refresh_rules
  if (is.null(f) || !is.function(f)) return("ALL")
  fml <- names(formals(f))
  args <- list(entity = entity, last_event = ev, changes = changes)
  if ("ctx" %in% fml) args$ctx <- ctx
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

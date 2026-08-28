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
#' @section Grouped decision dispatch:
#' For an Engine assembled with `schema$decision_groups`, raw direct/group
#' overlap is rejected before transition. After the event transition is applied
#' once, Core freezes ordinary eligibility followed by grouped-member
#' eligibility. Conditions are evaluated in ordinary schema order, then group
#' declaration and member order, before any policy call. Core dispatches ordinary
#' policies first and then each fired group in declaration order. A non-empty
#' eligible group receives exactly one `policy$propose_plan()` call; an empty
#' eligible group skips policy.
#'
#' A grouped [DecisionPlan()] must name every and only eligible member and is
#' validated completely, including every pending-slot outcome, before one
#' group-level commit. This atomic boundary coordinates action selection and
#' staging only; constituent actions later arbitrate and realize independently.
#' Ordinary decisions and separate groups are independent local boundaries, not
#' one event-wide transaction.
#'
#' @section Grouped trajectory records:
#' With trajectory logging enabled, a grouped activation emits ordinary-style
#' leaf rows, not a synthetic parent row. Every eligible member gets a row,
#' including an explicit `NULL` selection; an ineligible member gets a veto row
#' only when its leaf declares `audit = TRUE`. All rows from one firing share
#' `grouped_decision_point_id` and a deterministic run-local
#' `group_activation_id`. A zero-eligible firing still advances activation
#' identity and emits its opted-in veto rows, but has no policy call or plan
#' metadata. Plan metadata remains opaque and is retained only in raw records.
#'
#' @section Failure and partial progress:
#' Ambiguous raw activation is rejected before transition. Otherwise the
#' triggering transition and atomic [Entity] update occur before decision
#' conditions and policies. A condition, policy, or plan error therefore stops
#' the run without rolling back that triggering event. A failing action handler
#' stops before its action event or state effect is committed. Ordinary work and
#' each accepted group are independent local boundaries, so a later group error
#' does not roll back an earlier policy call, diagnostic, or pending-slot commit.
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

      fired_decision_groups <- function(schema, event, v2_mode = FALSE) {
        if (!isTRUE(v2_mode)) return(list())
        if (is.null(schema) || is.null(schema$decision_groups) ||
            length(schema$decision_groups) == 0L) return(list())
        out <- list()
        for (group in schema$decision_groups) {
          if (.group_fires(group, event)) out[[length(out) + 1L]] <- group
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
      # Group activation identity is run-local and deliberately independent of
      # the RNG stream. The counter advances for every unambiguous fired group,
      # even when logging is disabled or that activation emits no leaf rows.
      group_activation_count <- 0L
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

      # Allocate trajectory emitters once per run, not once per event. Their
      # first branch keeps the common no-logging path to a single NULL check.
      append_trajectory_record <- function(dp,
                                           selected_action,
                                           condition_met,
                                           entity,
                                           ev,
                                           state_before,
                                           state_after,
                                           grouped_decision_point_id = NULL,
                                           group_activation_id = NULL,
                                           decision_plan_metadata = NULL) {
        if (is.null(traj_cfg)) return(invisible(NULL))
        observation <- decision_observation(dp, entity)
        tr <- TrajectoryRecord(
          run_id = run_id,
          entity_id = entity_id,
          t = entity$last_time,
          decision_point_id = dp$id,
          observation = observation,
          realized_event = ev,
          candidate_actions = dp$allowed_actions,
          proposed_actions = if (!is.null(selected_action)) list(selected_action) else list(),
          selected_action = selected_action,
          state_before = state_before,
          state_after = state_after,
          condition_met = condition_met,
          reward = NULL,
          grouped_decision_point_id = grouped_decision_point_id,
          group_activation_id = group_activation_id,
          decision_plan_metadata = decision_plan_metadata
        )
        trajectory_accum[[length(trajectory_accum) + 1L]] <<-
          as_plain_trajectory_record(tr)
        invisible(NULL)
      }

      emit_group_trajectory <- function(activation,
                                        plan,
                                        entity,
                                        ev,
                                        state_before,
                                        state_after) {
        if (is.null(traj_cfg)) return(invisible(NULL))
        group <- activation$group
        eligible_ids <- names(activation$eligible)
        metadata <- if (is.null(plan)) NULL else plan$metadata

        # Preserve the group's canonical member order, interleaving eligible
        # rows and opted-in veto rows exactly as the leaves were declared.
        for (member_id in group$members) {
          dp <- dp_map[[member_id]]
          if (member_id %in% eligible_ids) {
            selected_action <- plan$selections[[member_id]]
            append_trajectory_record(
              dp = dp,
              selected_action = selected_action,
              condition_met = if (is.null(dp$condition)) NULL else TRUE,
              entity = entity,
              ev = ev,
              state_before = state_before,
              state_after = state_after,
              grouped_decision_point_id = group$id,
              group_activation_id = activation$activation_id,
              decision_plan_metadata = metadata
            )
          } else if (isTRUE(dp$audit)) {
            append_trajectory_record(
              dp = dp,
              selected_action = NULL,
              condition_met = FALSE,
              entity = entity,
              ev = ev,
              state_before = state_before,
              state_after = state_after,
              grouped_decision_point_id = group$id,
              group_activation_id = activation$activation_id,
              decision_plan_metadata = metadata
            )
          }
        }
        invisible(NULL)
      }

      proposals <- .call_propose_events(self$bundle, entity, sim_ctx = sim_ctx, param_ctx = param_ctx)

      # Engine-owned store of pending policy actions, keyed by decision point id.
      # This is deliberately separate from `proposals`: `proposals` is owned by the
      # model and rewritten by refresh_rules()/propose_events(), whereas pending
      # actions are owned by the engine, survive every refresh, and are retired by
      # the engine itself once realized. Each decision point holds at most one.
      action_proposals <- list()

      # Ordinary decisions retain their established sequential, per-slot D1
      # behavior. In a mixed activation this resolver runs before grouped
      # consultations so a later independent group failure does not suppress
      # earlier ordinary warnings or local pending-slot work. In a no-group
      # event it remains at the historical post-refresh location below.
      merge_ordinary_action_props <- function(action_props) {
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
        invisible(NULL)
      }

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
        fired_groups <- fired_decision_groups(self$.schema, ev, self$.v2_mode)
        .assert_unambiguous_decision_activations(fired_dps, fired_groups, ev)
        group_activation_ids <- character(length(fired_groups))
        if (length(fired_groups) > 0L) {
          for (group_i in seq_along(fired_groups)) {
            group_activation_count <<- group_activation_count + 1L
            group_activation_ids[[group_i]] <- paste0(
              "group_activation_", group_activation_count
            )
          }
        }
        has_decision_activation <- length(fired_dps) > 0L || length(fired_groups) > 0L
        state_before <- if (has_decision_activation) {
          capture_trajectory_state(entity, traj_cfg, when = "before")
        } else {
          NULL
        }

        if (!is.null(action_handler)) {
          changes <- .call_action_handler(action_handler, entity, ev, param_ctx = param_ctx)
        } else {
          changes <- .call_transition(self$bundle, entity, ev, sim_ctx = sim_ctx, param_ctx = param_ctx)
        }

        entity$update(time = ev$time_next, event_type = ev$event_type, changes = changes)

        # Once an action has updated the Entity it no longer occupies its
        # pending slot. Retire it before any decision preflight so both ordinary
        # and grouped paths may re-propose for that leaf without a false
        # conflict. Raw activation overlap was already rejected above, before
        # this engine-owned mutation.
        if (identical(ev_source, "action")) {
          action_proposals[[ev_key]] <<- NULL
        }

        state_after <- if (has_decision_activation) {
          capture_trajectory_state(entity, traj_cfg, when = "after")
        } else {
          NULL
        }

        # Freeze every post-transition eligibility result before the first
        # policy callback. Canonical order is ordinary schema order, followed
        # by fired groups in schema order and their members in declaration
        # order. Overlap has already been rejected, so each activation path is
        # evaluated exactly once.
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

        group_activations <- vector("list", length(fired_groups))
        if (length(fired_groups) > 0L) {
          for (group_i in seq_along(fired_groups)) {
            group <- fired_groups[[group_i]]
            eligible <- list()
            ineligible <- list()
            for (member_id in group$members) {
              dp <- dp_map[[member_id]]
              cond_met <- .evaluate_decision_condition(dp, entity)
              if (cond_met) {
                eligible[[length(eligible) + 1L]] <- dp
              } else {
                ineligible[[length(ineligible) + 1L]] <- dp
              }
            }
            if (length(eligible) > 0L) {
              names(eligible) <- vapply(eligible, `[[`, character(1), "id")
            }
            if (length(ineligible) > 0L) {
              names(ineligible) <- vapply(ineligible, `[[`, character(1), "id")
            }
            group_activations[[group_i]] <- list(
              group = group,
              activation_id = group_activation_ids[[group_i]],
              eligible = eligible,
              ineligible = ineligible
            )
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

        ordinary_actions_merged_early <- length(fired_groups) > 0L
        if (ordinary_actions_merged_early) {
          merge_ordinary_action_props(action_props)
        }

        # Ordinary activations form their own established diagnostic boundary.
        # Emit them before independent grouped consultations so a later group
        # failure does not suppress already-completed ordinary audit work.
        if (!is.null(traj_cfg)) {
          for (dp in active_dps) {
            append_trajectory_record(
              dp = dp,
              selected_action = selected_actions[[dp$id]],
              condition_met = if (is.null(dp$condition)) NULL else TRUE,
              entity = entity,
              ev = ev,
              state_before = state_before,
              state_after = state_after
            )
          }
          for (dp in vetoed_dps) {
            append_trajectory_record(
              dp = dp,
              selected_action = NULL,
              condition_met = FALSE,
              entity = entity,
              ev = ev,
              state_before = state_before,
              state_after = state_after
            )
          }
        }

        # Grouped consultations follow all ordinary policy callbacks. Each
        # non-empty eligible set receives exactly one strict propose_plan()
        # call, then one pure pending-store preflight and at most one commit.
        # Empty eligible sets deliberately skip policy.
        if (length(group_activations) > 0L) {
          for (group_i in seq_along(group_activations)) {
            activation <- group_activations[[group_i]]
            group <- activation$group
            eligible <- activation$eligible
            if (length(eligible) == 0L) {
              emit_group_trajectory(
                activation,
                plan = NULL,
                entity = entity,
                ev = ev,
                state_before = state_before,
                state_after = state_after
              )
              next
            }

            plan <- .call_group_policy(
              self$.policy,
              group = group,
              eligible_decision_points = eligible,
              entity = entity,
              sim_ctx = sim_ctx,
              param_ctx = param_ctx
            )
            plan <- .validate_group_decision_plan(
              plan,
              group = group,
              eligible_decision_points = eligible,
              current_time = entity$last_time,
              event_catalog = model_event_catalog
            )
            preflight <- .preflight_group_pending_actions(
              plan,
              group = group,
              eligible_decision_points = eligible,
              pending_actions = action_proposals
            )

            if (length(preflight$warning_ids) > 0L) {
              warning(
                sprintf(
                  "GroupedDecisionPoint('%s') replaced still-pending actions for member DecisionPoint id(s) {%s}. Set on_pending_action = 'replace' to declare replacement intentional, or 'keep' to preserve existing actions.",
                  group$id,
                  paste(preflight$warning_ids, collapse = ", ")
                ),
                call. = FALSE
              )
            }
            action_proposals <<- preflight$candidate
            activation$plan <- plan
            activation$pending_outcomes <- preflight$outcomes
            group_activations[[group_i]] <- activation

            # A group's accepted plan, pending-store commit, and audit rows are
            # one local boundary. Later independent group errors do not roll
            # back this activation's diagnostics or pending-slot work.
            emit_group_trajectory(
              activation,
              plan = plan,
              entity = entity,
              ev = ev,
              state_before = state_before,
              state_after = state_after
            )
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
        # realized action was retired immediately after Entity update, so a
        # decision point re-proposing right after its own action fired is not a
        # conflict.
        if (!ordinary_actions_merged_early) {
          merge_ordinary_action_props(action_props)
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

# Test whether a GroupedDecisionPoint's raw trigger fires. Group triggers use
# the same pre-transition matching contract as ordinary DecisionPoint triggers.
.group_fires <- function(group, event) {
  stopifnot(inherits(group, "GroupedDecisionPoint"))
  if (is.function(group$trigger)) {
    isTRUE(group$trigger(event))
  } else {
    isTRUE(event$event_type %in% group$trigger)
  }
}

# Reject a leaf activated through more than one raw path before any transition,
# Entity mutation, condition, or policy callback. Conditions cannot resolve a
# structural ambiguity because they are intentionally post-transition.
.assert_unambiguous_decision_activations <- function(fired_dps, fired_groups, event) {
  direct_ids <- if (length(fired_dps) == 0L) {
    character()
  } else {
    vapply(fired_dps, `[[`, character(1), "id")
  }
  grouped_ids <- if (length(fired_groups) == 0L) {
    character()
  } else {
    unlist(lapply(fired_groups, `[[`, "members"), use.names = FALSE)
  }
  activated_ids <- c(direct_ids, grouped_ids)
  overlapping_ids <- unique(activated_ids[duplicated(activated_ids)])
  if (length(overlapping_ids) == 0L) return(invisible(TRUE))

  describe_paths <- function(id) {
    paths <- character()
    if (id %in% direct_ids) paths <- c(paths, "direct trigger")
    for (group in fired_groups) {
      if (id %in% group$members) {
        paths <- c(paths, sprintf("GroupedDecisionPoint('%s')", group$id))
      }
    }
    sprintf("%s via %s", id, paste(paths, collapse = " + "))
  }
  event_label <- if (is.character(event$event_type) &&
                     length(event$event_type) == 1L &&
                     !is.na(event$event_type)) {
    event$event_type
  } else {
    "<unknown>"
  }
  stop(
    sprintf(
      "Engine$run(): ambiguous decision activation for raw event '%s': %s. Each leaf may activate through only one direct or grouped path per event.",
      event_label,
      paste(vapply(overlapping_ids, describe_paths, character(1)), collapse = "; ")
    ),
    call. = FALSE
  )
}

# One strict grouped-policy consultation. A missing method is rejected by
# load_model() and checked again here because Engine fields are publicly
# mutable after assembly.
.call_group_policy <- function(policy,
                               group,
                               eligible_decision_points,
                               entity,
                               sim_ctx = NULL,
                               param_ctx = NULL) {
  if (!is.list(policy) || !is.function(policy$propose_plan)) {
    stop(
      sprintf(
        "GroupedDecisionPoint('%s') requires policy$propose_plan().",
        group$id
      ),
      call. = FALSE
    )
  }

  f <- policy$propose_plan
  fml <- names(formals(f))
  args <- list(
    grouped_decision_point = group,
    eligible_decision_points = eligible_decision_points,
    entity = entity
  )
  if ("sim_ctx" %in% fml) args$sim_ctx <- sim_ctx
  if ("param_ctx" %in% fml) args$param_ctx <- param_ctx

  tryCatch(
    do.call(f, args),
    error = function(e) {
      stop(
        sprintf(
          "policy$propose_plan() errored for GroupedDecisionPoint('%s'): %s",
          group$id,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

# Validate one grouped ActionEvent before pending-store preflight. Ordinary
# decisions retain their established warn-and-ignore path; the coordinated path
# is strict so one malformed member rejects the whole plan.
.validate_group_action_event <- function(action,
                                         group,
                                         dp,
                                         current_time,
                                         event_catalog = NULL) {
  context <- sprintf(
    "GroupedDecisionPoint('%s') selection for DecisionPoint('%s')",
    group$id,
    dp$id
  )
  if (!inherits(action, "ActionEvent") || !is.list(action)) {
    stop(context, " must be an ActionEvent or explicit NULL.", call. = FALSE)
  }
  action_fields <- names(action)
  expected_fields <- c(
    "action_type", "event_type", "time_next", "decision_point_id",
    "params", "metadata"
  )
  if (is.null(action_fields) || anyNA(action_fields) ||
      !identical(sort(action_fields), sort(expected_fields))) {
    stop(
      context,
      " is a malformed ActionEvent; expected exactly one `action_type`, `event_type`, `time_next`, `decision_point_id`, `params`, and `metadata` field.",
      call. = FALSE
    )
  }
  if (!is.character(action$action_type) || length(action$action_type) != 1L ||
      is.na(action$action_type) || !nzchar(action$action_type)) {
    stop(context, " has an invalid `action_type`.", call. = FALSE)
  }
  if (!is.character(action$event_type) || length(action$event_type) != 1L ||
      is.na(action$event_type) || !nzchar(action$event_type) ||
      !identical(action$event_type, action$action_type)) {
    stop(
      context,
      " must have one non-empty `event_type` identical to `action_type`.",
      call. = FALSE
    )
  }
  if (!is.numeric(action$time_next) || length(action$time_next) != 1L ||
      !is.finite(action$time_next)) {
    stop(context, " has an invalid finite numeric `time_next`.", call. = FALSE)
  }
  if (action$time_next < current_time) {
    stop(
      sprintf(
        "%s schedules t=%g before the current model time t=%g.",
        context, action$time_next, current_time
      ),
      call. = FALSE
    )
  }
  if (!is.null(dp$allowed_actions) &&
      !(action$action_type %in% dp$allowed_actions)) {
    stop(
      sprintf(
        "%s returned action_type '%s', which is not in allowed_actions {%s}.",
        context,
        action$action_type,
        paste(dp$allowed_actions, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.null(event_catalog) && !(action$event_type %in% event_catalog)) {
    stop(
      sprintf(
        "%s has event_type '%s' not declared in the model event catalog.",
        context, action$event_type
      ),
      call. = FALSE
    )
  }
  if (!is.null(action$params) && !is.list(action$params)) {
    stop(context, " has invalid `params`; expected a list or NULL.", call. = FALSE)
  }
  if (!is.null(action$metadata) && !is.list(action$metadata)) {
    stop(context, " has invalid `metadata`; expected a list or NULL.", call. = FALSE)
  }
  if (!is.null(action$decision_point_id) &&
      (!is.character(action$decision_point_id) ||
       length(action$decision_point_id) != 1L ||
       is.na(action$decision_point_id) ||
       !nzchar(action$decision_point_id))) {
    stop(
      context,
      " has invalid `decision_point_id`; expected one non-empty character value or NULL.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Validate plan shape/completeness and normalize ActionEvent provenance. The
# returned selection list is reordered to canonical eligible-member order so
# pending resolution never depends on policy list order.
.validate_group_decision_plan <- function(plan,
                                          group,
                                          eligible_decision_points,
                                          current_time,
                                          event_catalog = NULL) {
  context <- sprintf("GroupedDecisionPoint('%s')", group$id)
  if (!inherits(plan, "DecisionPlan") || !is.list(plan)) {
    stop(
      context,
      " policy$propose_plan() result must be a DecisionPlan, not NULL or another object.",
      call. = FALSE
    )
  }

  plan_fields <- names(plan)
  expected_fields <- c("selections", "metadata")
  if (is.null(plan_fields) || anyNA(plan_fields) ||
      !identical(sort(plan_fields), sort(expected_fields))) {
    stop(
      context,
      " returned a malformed DecisionPlan; expected exactly `selections` and `metadata` fields.",
      call. = FALSE
    )
  }
  if (!is.list(plan$selections) || length(plan$selections) == 0L) {
    stop(context, " returned malformed `DecisionPlan$selections`.", call. = FALSE)
  }
  selection_ids <- names(plan$selections)
  if (is.null(selection_ids) || anyNA(selection_ids) || any(!nzchar(selection_ids)) ||
      anyDuplicated(selection_ids)) {
    stop(
      context,
      " requires unique, non-empty names on every DecisionPlan selection.",
      call. = FALSE
    )
  }

  eligible_ids <- names(eligible_decision_points)
  missing_ids <- setdiff(eligible_ids, selection_ids)
  extra_ids <- setdiff(selection_ids, eligible_ids)
  if (length(missing_ids) > 0L || length(extra_ids) > 0L ||
      length(selection_ids) != length(eligible_ids)) {
    details <- character()
    if (length(missing_ids) > 0L) {
      details <- c(details, sprintf("missing {%s}", paste(missing_ids, collapse = ", ")))
    }
    if (length(extra_ids) > 0L) {
      details <- c(details, sprintf("extra {%s}", paste(extra_ids, collapse = ", ")))
    }
    stop(
      sprintf(
        "%s returned an incomplete DecisionPlan: selections must name every and only eligible member (%s).",
        context,
        paste(details, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(plan$metadata)) {
    if (!is.list(plan$metadata)) {
      stop(context, " returned invalid DecisionPlan metadata; expected a named list or NULL.", call. = FALSE)
    }
    if (length(plan$metadata) > 0L) {
      metadata_names <- names(plan$metadata)
      if (is.null(metadata_names) || anyNA(metadata_names) ||
          any(!nzchar(metadata_names)) || anyDuplicated(metadata_names)) {
        stop(
          context,
          " returned invalid DecisionPlan metadata names; expected unique non-empty names.",
          call. = FALSE
        )
      }
    }
  }

  normalized <- vector("list", length(eligible_ids))
  names(normalized) <- eligible_ids
  for (i in seq_along(eligible_ids)) {
    member_id <- eligible_ids[[i]]
    selection <- plan$selections[[match(member_id, selection_ids)]]
    if (is.null(selection)) {
      normalized[i] <- list(NULL)
      next
    }
    if (!inherits(selection, "ActionEvent") || !is.list(selection)) {
      stop(
        sprintf(
          "%s selection for DecisionPoint('%s') must be an ActionEvent or explicit NULL.",
          context, member_id
        ),
        call. = FALSE
      )
    }
    .validate_group_action_event(
      selection,
      group = group,
      dp = eligible_decision_points[[member_id]],
      current_time = current_time,
      event_catalog = event_catalog
    )
    selection <- .normalize_action_provenance(
      selection,
      decision_point_id = member_id,
      source = sprintf("policy$propose_plan() for GroupedDecisionPoint('%s')", group$id)
    )
    normalized[[i]] <- selection
  }

  plan$selections <- normalized
  plan
}

# Pure pending-store preflight for one complete plan. It returns a candidate
# store and diagnostics but never mutates the live store or emits warnings.
.preflight_group_pending_actions <- function(plan,
                                             group,
                                             eligible_decision_points,
                                             pending_actions) {
  candidate <- pending_actions
  warning_ids <- character()
  outcomes <- stats::setNames(
    character(length(eligible_decision_points)),
    names(eligible_decision_points)
  )

  for (i in seq_along(eligible_decision_points)) {
    dp <- eligible_decision_points[[i]]
    member_id <- dp$id
    selection <- plan$selections[[i]]
    if (is.null(selection)) {
      outcomes[[member_id]] <- "no_action"
      next
    }

    pending <- candidate[[member_id]]
    if (is.null(pending)) {
      candidate[[member_id]] <- selection
      outcomes[[member_id]] <- "stage"
      next
    }

    mode <- dp$on_pending_action
    if (is.null(mode)) mode <- "warn"
    if (identical(mode, "keep")) {
      outcomes[[member_id]] <- "keep"
      next
    }
    if (identical(mode, "error")) {
      stop(
        sprintf(
          "GroupedDecisionPoint('%s') cannot stage its complete plan: DecisionPoint('%s') selected an action for t=%g while its previous action for t=%g is still pending and on_pending_action = 'error'.",
          group$id, member_id, selection$time_next, pending$time_next
        ),
        call. = FALSE
      )
    }
    if (!(mode %in% c("replace", "warn"))) {
      stop(
        sprintf(
          "GroupedDecisionPoint('%s') found unsupported on_pending_action mode '%s' for DecisionPoint('%s').",
          group$id, mode, member_id
        ),
        call. = FALSE
      )
    }

    candidate[[member_id]] <- selection
    outcomes[[member_id]] <- mode
    if (identical(mode, "warn")) warning_ids <- c(warning_ids, member_id)
  }

  list(
    candidate = candidate,
    warning_ids = warning_ids,
    outcomes = outcomes
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

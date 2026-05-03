# decision_points.R --------------------------------------------------------
#
# Structures for the action/policy layer introduced in v2.0.0:
#
#   DecisionPoint   -- declared in Schema; maps trigger events to policy hooks
#   ActionEvent     -- timeline-native action proposed by a policy
#   TrajectoryRecord -- engine-owned audit record emitted at decision points
#
# Summary function helpers for state_before / state_after capture.
# --------------------------------------------------------------------------


# DecisionPoint -------------------------------------------------------------

#' Construct a DecisionPoint specification
#'
#' Declares a point in the simulation where a policy may be consulted. Decision
#' points are declared in the Schema (`schema$decision_points`), not inferred at
#' runtime.
#'
#' @param id Character scalar; unique identifier (e.g., `"post_dropoff"`).
#' @param trigger Character vector of event types and/or a predicate function
#'   `function(event)` that returns `TRUE` when the decision point fires.
#' @param allowed_actions Optional character vector of named action types. If
#'   `NULL`, the policy is unconstrained.
#' @param observation_fn Optional function `function(entity)` that computes the
#'   observable state presented to the policy. Defaults to a full entity
#'   snapshot when `NULL`.
#' @param label Optional human-readable description.
#'
#' @return A list of class `"DecisionPoint"`.
#'
#' @export
DecisionPoint <- function(id,
                          trigger,
                          allowed_actions = NULL,
                          observation_fn  = NULL,
                          label           = NULL) {
  if (missing(id) || !is.character(id) || length(id) != 1L || !nzchar(id)) {
    stop("DecisionPoint: `id` must be a non-empty character scalar.", call. = FALSE)
  }
  if (missing(trigger)) {
    stop("DecisionPoint: `trigger` must be a character vector or predicate function.", call. = FALSE)
  }
  if (!is.character(trigger) && !is.function(trigger)) {
    stop("DecisionPoint: `trigger` must be a character vector (event type(s)) or a function.", call. = FALSE)
  }
  if (is.character(trigger) && (length(trigger) == 0L || any(!nzchar(trigger)))) {
    stop("DecisionPoint: `trigger` character vector must have at least one non-empty element.", call. = FALSE)
  }
  if (!is.null(allowed_actions)) {
    if (!is.character(allowed_actions) || length(allowed_actions) == 0L || any(!nzchar(allowed_actions))) {
      stop("DecisionPoint: `allowed_actions` must be a non-empty character vector or NULL.", call. = FALSE)
    }
  }
  if (!is.null(observation_fn) && !is.function(observation_fn)) {
    stop("DecisionPoint: `observation_fn` must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("DecisionPoint: `label` must be a character scalar or NULL.", call. = FALSE)
  }

  structure(
    list(
      id              = id,
      trigger         = trigger,
      allowed_actions = allowed_actions,
      observation_fn  = observation_fn,
      label           = label
    ),
    class = "DecisionPoint"
  )
}

#' Test whether a DecisionPoint fires for a given event
#'
#' Used internally by the Engine to detect decision points during the simulation
#' loop.
#'
#' @param dp A `DecisionPoint` object.
#' @param event An event list with at least `event_type`.
#'
#' @return Logical scalar.
#'
#' @export
dp_fires <- function(dp, event) {
  stopifnot(inherits(dp, "DecisionPoint"))
  if (is.function(dp$trigger)) {
    isTRUE(dp$trigger(event))
  } else {
    isTRUE(event$event_type %in% dp$trigger)
  }
}

#' @export
print.DecisionPoint <- function(x, ...) {
  cat("<DecisionPoint:", x$id, ">\n")
  if (is.character(x$trigger)) {
    cat("  trigger        :", paste(x$trigger, collapse = ", "), "\n")
  } else {
    cat("  trigger        : (predicate function)\n")
  }
  cat("  allowed_actions:", if (is.null(x$allowed_actions)) "(unconstrained)" else paste(x$allowed_actions, collapse = ", "), "\n")
  cat("  label          :", if (is.null(x$label)) "(none)" else x$label, "\n")
  invisible(x)
}


# ActionEvent ---------------------------------------------------------------

#' Construct an ActionEvent
#'
#' Timeline-native action proposed by a policy at a decision point. ActionEvents
#' enter the same arbitration and refresh lifecycle as all other events — no
#' side-channel state mutation.
#'
#' @param action_type Character scalar; must match an `allowed_actions` entry if
#'   the decision point declared any.
#' @param time_next Numeric scalar; realization time on the canonical timeline.
#' @param decision_point_id Character scalar; which decision point produced this
#'   action.
#' @param params Optional named list of action-specific parameters.
#' @param metadata Optional named list for policy provenance or audit fields.
#'
#' @return A list of class `"ActionEvent"`.
#'
#' @export
ActionEvent <- function(action_type,
                        time_next,
                        decision_point_id,
                        params   = NULL,
                        metadata = NULL) {
  if (missing(action_type) || !is.character(action_type) || length(action_type) != 1L || !nzchar(action_type)) {
    stop("ActionEvent: `action_type` must be a non-empty character scalar.", call. = FALSE)
  }
  time_next <- suppressWarnings(as.numeric(time_next))
  if (length(time_next) != 1L || !is.finite(time_next)) {
    stop("ActionEvent: `time_next` must be a finite numeric scalar.", call. = FALSE)
  }
  if (missing(decision_point_id) || !is.character(decision_point_id) || length(decision_point_id) != 1L || !nzchar(decision_point_id)) {
    stop("ActionEvent: `decision_point_id` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.null(params) && !is.list(params)) {
    stop("ActionEvent: `params` must be a named list or NULL.", call. = FALSE)
  }
  if (!is.null(metadata) && !is.list(metadata)) {
    stop("ActionEvent: `metadata` must be a named list or NULL.", call. = FALSE)
  }

  structure(
    list(
      action_type       = action_type,
      time_next         = time_next,
      decision_point_id = decision_point_id,
      params            = params,
      metadata          = metadata
    ),
    class = "ActionEvent"
  )
}

#' @export
print.ActionEvent <- function(x, ...) {
  cat("<ActionEvent>\n")
  cat("  action_type      :", x$action_type, "\n")
  cat("  time_next        :", x$time_next, "\n")
  cat("  decision_point_id:", x$decision_point_id, "\n")
  invisible(x)
}


# State summary helpers -----------------------------------------------------

#' Default state summary function for TrajectoryRecord
#'
#' Captures a compact named list of all current variable values from an entity.
#' Used as the default `summary_fn` when `state_before` or `state_after` is
#' requested in a TrajectoryLogger with `detail = "summary"`.
#'
#' Implementors may supply a custom summary function with the same signature:
#' `function(entity, ...)` returning a named list.
#'
#' @param entity An `Entity` object.
#' @param ... Reserved for future arguments (ignored).
#'
#' @return A named list of variable values.
#'
#' @export
state_summary_default <- function(entity, ...) {
  # entity$current is the live named list of current variable values.
  s <- entity$current
  if (is.null(s) || !is.list(s)) return(list())
  as.list(s)
}


# TrajectoryRecord ----------------------------------------------------------

#' Construct a TrajectoryRecord
#'
#' Engine-owned audit record emitted at each decision point when a
#' `TrajectoryLogger` is configured. This is the canonical surface for policy
#' evaluation, audit, and RL reward computation.
#'
#' `state_before` and `state_after` are `NULL` by default. Supply a
#' `summary_fn` to the TrajectoryLogger to enable summary-level or full capture.
#'
#' @param run_id Character scalar; run identifier (from `SimContext`).
#' @param entity_id Character scalar; entity identifier.
#' @param t Numeric scalar; time at which the decision point fired.
#' @param decision_point_id Character scalar; which decision point produced
#'   this record.
#' @param observation Named list; output of the decision point's
#'   `observation_fn`.
#' @param realized_event The event record that triggered the decision point.
#' @param candidate_actions Optional character vector of actions presented to
#'   the policy.
#' @param proposed_actions Optional list of actions proposed by the policy.
#' @param selected_action Optional `ActionEvent`; the action realized on the
#'   timeline.
#' @param state_before Optional named list; entity state before the event.
#'   `NULL` unless the TrajectoryLogger is configured to capture it.
#' @param state_after Optional named list; entity state after the transition.
#'   `NULL` unless the TrajectoryLogger is configured to capture it.
#' @param reward Optional numeric or named list; reward signal(s).
#'
#' @return A list of class `"TrajectoryRecord"`.
#'
#' @export
TrajectoryRecord <- function(run_id,
                             entity_id,
                             t,
                             decision_point_id,
                             observation,
                             realized_event,
                             candidate_actions = NULL,
                             proposed_actions  = NULL,
                             selected_action   = NULL,
                             state_before      = NULL,
                             state_after       = NULL,
                             reward            = NULL) {
  if (!is.character(run_id) || length(run_id) != 1L || !nzchar(run_id)) {
    stop("TrajectoryRecord: `run_id` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(entity_id) || length(entity_id) != 1L || !nzchar(entity_id)) {
    stop("TrajectoryRecord: `entity_id` must be a non-empty character scalar.", call. = FALSE)
  }
  t <- suppressWarnings(as.numeric(t))
  if (length(t) != 1L || !is.finite(t)) {
    stop("TrajectoryRecord: `t` must be a finite numeric scalar.", call. = FALSE)
  }
  if (!is.character(decision_point_id) || length(decision_point_id) != 1L || !nzchar(decision_point_id)) {
    stop("TrajectoryRecord: `decision_point_id` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.list(observation)) {
    stop("TrajectoryRecord: `observation` must be a named list.", call. = FALSE)
  }
  if (!is.null(candidate_actions) && !is.character(candidate_actions)) {
    stop("TrajectoryRecord: `candidate_actions` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(selected_action) && !inherits(selected_action, "ActionEvent")) {
    stop("TrajectoryRecord: `selected_action` must be an ActionEvent or NULL.", call. = FALSE)
  }

  structure(
    list(
      run_id            = run_id,
      entity_id         = entity_id,
      t                 = t,
      decision_point_id = decision_point_id,
      observation       = observation,
      realized_event    = realized_event,
      candidate_actions = candidate_actions,
      proposed_actions  = proposed_actions,
      selected_action   = selected_action,
      state_before      = state_before,
      state_after       = state_after,
      reward            = reward
    ),
    class = "TrajectoryRecord"
  )
}

#' @export
print.TrajectoryRecord <- function(x, ...) {
  cat("<TrajectoryRecord>\n")
  cat("  run_id            :", x$run_id, "\n")
  cat("  entity_id         :", x$entity_id, "\n")
  cat("  t                 :", x$t, "\n")
  cat("  decision_point_id :", x$decision_point_id, "\n")
  cat("  observation fields:", length(x$observation), "\n")
  cat("  selected_action   :", if (is.null(x$selected_action)) "(none)" else x$selected_action$action_type, "\n")
  invisible(x)
}

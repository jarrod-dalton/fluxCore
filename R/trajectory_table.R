#' Compile trajectory records into a data frame
#'
#' Takes the list of trajectory records from an engine run and returns a tidy
#' data frame with one row per decision point firing.
#'
#' @param records List of trajectory records (from `engine$run(...)$trajectory_records`).
#' @param vars Character vector of state variable names to extract from
#'   `state_before` and `state_after`. If `NULL` (default), all variables in
#'   `state_before` are included.
#'
#' @return A data.frame with columns: `run_id`, `entity_id`, `t`,
#'   `decision_point_id`, `grouped_decision_point_id`, `group_activation_id`,
#'   `trigger_event`, `selected_action`, `condition_met`, plus
#'   `<var>_before` and `<var>_after` for each requested variable. Grouped ids
#'   are `NA` for ordinary or legacy records. Opaque decision-plan metadata
#'   remains available only in raw trajectory records.
#'
#' @export
trajectory_table <- function(records, vars = NULL) {
  if (!is.list(records) || length(records) == 0L) {
    out <- data.frame(
      run_id = character(0),
      entity_id = character(0),
      t = numeric(0),
      decision_point_id = character(0),
      grouped_decision_point_id = character(0),
      group_activation_id = character(0),
      trigger_event = character(0),
      selected_action = character(0),
      condition_met = logical(0),
      stringsAsFactors = FALSE
    )
    if (!is.null(vars)) {
      for (vn in vars) {
        out[[paste0(vn, "_before")]] <- logical(0)
        out[[paste0(vn, "_after")]] <- logical(0)
      }
    }
    return(out)
  }

  rows <- lapply(records, function(tr) {
    # selected_action is the policy's chosen action (may be NULL if no action).
    # realized_event is the triggering event that fired the decision point.
    action <- if (!is.null(tr$selected_action)) {
      if (is.list(tr$selected_action)) tr$selected_action$action_type else NA_character_
    } else {
      NA_character_
    }
    grouped_decision_point_id <- tr$grouped_decision_point_id
    if (is.null(grouped_decision_point_id)) {
      grouped_decision_point_id <- NA_character_
    }
    group_activation_id <- tr$group_activation_id
    if (is.null(group_activation_id)) {
      group_activation_id <- NA_character_
    }

    row <- list(
      run_id            = tr$run_id,
      entity_id         = tr$entity_id,
      t                 = tr$t,
      decision_point_id = tr$decision_point_id,
      grouped_decision_point_id = as.character(grouped_decision_point_id),
      group_activation_id = as.character(group_activation_id),
      trigger_event     = if (!is.null(tr$realized_event)) tr$realized_event$event_type else NA_character_,
      selected_action   = action,
      condition_met     = if (is.null(tr$condition_met)) NA else tr$condition_met
    )

    # Extract before/after state
    v <- if (!is.null(vars)) vars else names(tr$state_before)
    if (!is.null(v) && !is.null(tr$state_before)) {
      for (vn in v) {
        val_b <- tr$state_before[[vn]]
        val_a <- tr$state_after[[vn]]
        row[[paste0(vn, "_before")]] <- if (is.null(val_b)) NA else val_b
        row[[paste0(vn, "_after")]]  <- if (is.null(val_a)) NA else val_a
      }
    }
    as.data.frame(row, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

# grouped_decisions.R ------------------------------------------------------
#
# Public declarations for coordinated policy consultation. Grouped decision
# execution is implemented by the Engine; this file owns only the declaration
# objects and the schema-level reference contract.
# --------------------------------------------------------------------------


# GroupedDecisionPoint ------------------------------------------------------

#' Construct a grouped decision point
#'
#' Declares one shared trigger that opens a coordinated policy consultation
#' across existing leaf [DecisionPoint()] objects. Group members are references
#' to ids in `schema$decision_points`; a grouped declaration does not copy leaf
#' action contracts or own pending-action slots.
#'
#' @param id Non-empty character scalar. Group and leaf ids share one
#'   schema-wide namespace.
#' @param trigger Non-empty character vector of event types or a predicate
#'   function `function(event)`. Unlike a group-only leaf, a grouped decision
#'   point must own a trigger.
#' @param members Character vector containing at least two distinct leaf
#'   decision-point ids. Declaration order is preserved and is the canonical
#'   member order.
#' @param label Optional human-readable character scalar.
#'
#' @return A list of class `"GroupedDecisionPoint"`.
#'
#' @export
GroupedDecisionPoint <- function(id, trigger, members, label = NULL) {
  if (missing(id) || !is.character(id) || length(id) != 1L ||
      is.na(id) || !nzchar(id)) {
    stop("GroupedDecisionPoint: `id` must be a non-empty character scalar.", call. = FALSE)
  }
  if (missing(trigger)) {
    stop("GroupedDecisionPoint: `trigger` must be supplied.", call. = FALSE)
  }
  if (is.null(trigger) || (!is.character(trigger) && !is.function(trigger))) {
    stop("GroupedDecisionPoint: `trigger` must be a character vector or a function; it cannot be NULL.", call. = FALSE)
  }
  if (is.character(trigger) &&
      (length(trigger) == 0L || anyNA(trigger) || any(!nzchar(trigger)))) {
    stop("GroupedDecisionPoint: `trigger` character vector must have at least one non-empty element.", call. = FALSE)
  }
  if (missing(members) || !is.character(members) || length(members) < 2L ||
      anyNA(members) || any(!nzchar(members))) {
    stop("GroupedDecisionPoint: `members` must contain at least two non-empty character ids.", call. = FALSE)
  }
  if (anyDuplicated(members)) {
    stop("GroupedDecisionPoint: `members` must contain distinct ids.", call. = FALSE)
  }
  if (!is.null(label) &&
      (!is.character(label) || length(label) != 1L || is.na(label))) {
    stop("GroupedDecisionPoint: `label` must be a character scalar or NULL.", call. = FALSE)
  }

  structure(
    list(
      id = id,
      trigger = trigger,
      members = members,
      label = label
    ),
    class = "GroupedDecisionPoint"
  )
}

#' @export
print.GroupedDecisionPoint <- function(x, ...) {
  cat("<GroupedDecisionPoint:", x$id, ">\n")
  if (is.character(x$trigger)) {
    cat("  trigger :", paste(x$trigger, collapse = ", "), "\n")
  } else {
    cat("  trigger : (predicate function)\n")
  }
  cat("  members :", paste(x$members, collapse = ", "), "\n")
  cat("  label   :", if (is.null(x$label)) "(none)" else x$label, "\n")
  invisible(x)
}


# DecisionPlan --------------------------------------------------------------

#' Construct a coordinated decision plan
#'
#' Represents one complete set of selections returned by a grouped policy
#' consultation. Each named entry is either one [ActionEvent()] or explicit
#' `NULL`, meaning that the eligible leaf was considered but no new action was
#' selected. Completeness against the eligible members is validated by the
#' Engine, which has the activation context.
#'
#' @param selections Non-empty named list with unique leaf decision-point ids.
#'   Every value must be an [ActionEvent()] or explicit `NULL`.
#' @param metadata Optional named list of compact plan-level provenance. Core
#'   treats these values as opaque audit information, never as execution input.
#'
#' @return A list of class `"DecisionPlan"`.
#'
#' @export
DecisionPlan <- function(selections, metadata = NULL) {
  if (missing(selections) || !is.list(selections) || length(selections) == 0L) {
    stop("DecisionPlan: `selections` must be a non-empty named list.", call. = FALSE)
  }
  selection_ids <- names(selections)
  if (is.null(selection_ids) || anyNA(selection_ids) || any(!nzchar(selection_ids))) {
    stop("DecisionPlan: `selections` must have one non-empty name for every entry.", call. = FALSE)
  }
  if (anyDuplicated(selection_ids)) {
    stop("DecisionPlan: `selections` names must be unique.", call. = FALSE)
  }
  valid_selection <- vapply(
    selections,
    function(x) is.null(x) || inherits(x, "ActionEvent"),
    logical(1)
  )
  if (any(!valid_selection)) {
    bad <- selection_ids[!valid_selection]
    stop(
      sprintf(
        "DecisionPlan: selection(s) {%s} must each be an ActionEvent or explicit NULL.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(metadata)) {
    if (!is.list(metadata)) {
      stop("DecisionPlan: `metadata` must be a named list or NULL.", call. = FALSE)
    }
    if (length(metadata) > 0L) {
      metadata_names <- names(metadata)
      if (is.null(metadata_names) || anyNA(metadata_names) || any(!nzchar(metadata_names))) {
        stop("DecisionPlan: `metadata` must have one non-empty name for every entry.", call. = FALSE)
      }
      if (anyDuplicated(metadata_names)) {
        stop("DecisionPlan: `metadata` names must be unique.", call. = FALSE)
      }
    }
  }

  structure(
    list(
      selections = selections,
      metadata = metadata
    ),
    class = "DecisionPlan"
  )
}

#' @export
print.DecisionPlan <- function(x, ...) {
  cat("<DecisionPlan>\n")
  cat("  selections:", length(x$selections), "\n")
  for (id in names(x$selections)) {
    selection <- x$selections[[id]]
    description <- if (is.null(selection)) {
      "(no new action)"
    } else {
      paste0(selection$action_type, " at t = ", format(selection$time_next, trim = TRUE))
    }
    cat("   ", id, ":", description, "\n")
  }
  metadata_names <- if (is.null(x$metadata) || length(x$metadata) == 0L) {
    "(none)"
  } else {
    paste(names(x$metadata), collapse = ", ")
  }
  cat("  metadata  :", metadata_names, "\n")
  invisible(x)
}


# Schema-level validation ---------------------------------------------------

# Validate the decision declaration graph shared by set_schema() and
# load_model(). Constructors own local validation; this helper repeats the
# structural fields needed to defend against hand-assembled or modified full
# schemas and then checks all cross-object references.
.validate_decision_schema_contract <- function(decision_points = NULL,
                                               decision_groups = NULL,
                                               caller = "set_schema") {
  prefix <- paste0(caller, "(): ")

  if (is.null(decision_points)) decision_points <- list()
  if (is.null(decision_groups)) decision_groups <- list()
  if (!is.list(decision_points)) {
    stop(prefix, "`schema$decision_points` must be a list or NULL.", call. = FALSE)
  }
  if (!is.list(decision_groups)) {
    stop(prefix, "`schema$decision_groups` must be a list or NULL.", call. = FALSE)
  }

  for (i in seq_along(decision_points)) {
    dp <- decision_points[[i]]
    if (!inherits(dp, "DecisionPoint") || !is.list(dp)) {
      stop(
        prefix, sprintf("`schema$decision_points[[%d]]` is not a DecisionPoint object.", i),
        call. = FALSE
      )
    }
    dp_names <- names(dp)
    if (is.null(dp_names) || anyNA(dp_names) || sum(dp_names == "id") != 1L ||
        sum(dp_names == "trigger") != 1L) {
      stop(
        prefix, sprintf("`schema$decision_points[[%d]]` must contain exactly one `id` and `trigger` field.", i),
        call. = FALSE
      )
    }
    if (!is.character(dp$id) || length(dp$id) != 1L || is.na(dp$id) || !nzchar(dp$id)) {
      stop(
        prefix, sprintf("`schema$decision_points[[%d]]$id` must be a non-empty character scalar.", i),
        call. = FALSE
      )
    }
    if (!is.null(dp$trigger) && !is.character(dp$trigger) && !is.function(dp$trigger)) {
      stop(
        prefix, sprintf("`schema$decision_points[[%d]]$trigger` must be a character vector, a function, or NULL.", i),
        call. = FALSE
      )
    }
    if (is.character(dp$trigger) &&
        (length(dp$trigger) == 0L || anyNA(dp$trigger) || any(!nzchar(dp$trigger)))) {
      stop(
        prefix, sprintf("`schema$decision_points[[%d]]$trigger` must contain non-empty event types.", i),
        call. = FALSE
      )
    }
  }

  leaf_only_fields <- c(
    "allowed_actions", "action_handlers", "condition", "audit",
    "on_pending_action", "observation_fn"
  )
  for (i in seq_along(decision_groups)) {
    group <- decision_groups[[i]]
    if (!inherits(group, "GroupedDecisionPoint") || !is.list(group)) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]` is not a GroupedDecisionPoint object.", i),
        call. = FALSE
      )
    }
    group_names <- names(group)
    required <- c("id", "trigger", "members")
    if (is.null(group_names) || anyNA(group_names) ||
        any(vapply(required, function(nm) sum(group_names == nm) != 1L, logical(1)))) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]` must contain exactly one `id`, `trigger`, and `members` field.", i),
        call. = FALSE
      )
    }
    if (!is.character(group$id) || length(group$id) != 1L ||
        is.na(group$id) || !nzchar(group$id)) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]$id` must be a non-empty character scalar.", i),
        call. = FALSE
      )
    }
    forbidden <- intersect(group_names, leaf_only_fields)
    if (length(forbidden) > 0L) {
      stop(
        prefix,
        sprintf(
          "GroupedDecisionPoint '%s' contains leaf-only field(s): %s.",
          group$id, paste(forbidden, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (is.null(group$trigger) ||
        (!is.character(group$trigger) && !is.function(group$trigger))) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]$trigger` must be a character vector or a function, not NULL.", i),
        call. = FALSE
      )
    }
    if (is.character(group$trigger) &&
        (length(group$trigger) == 0L || anyNA(group$trigger) || any(!nzchar(group$trigger)))) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]$trigger` must contain non-empty event types.", i),
        call. = FALSE
      )
    }
    if (!is.character(group$members) || length(group$members) < 2L ||
        anyNA(group$members) || any(!nzchar(group$members))) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]$members` must contain at least two non-empty character ids.", i),
        call. = FALSE
      )
    }
    if (anyDuplicated(group$members)) {
      stop(
        prefix, sprintf("`schema$decision_groups[[%d]]$members` must contain distinct ids.", i),
        call. = FALSE
      )
    }
  }

  leaf_ids <- vapply(decision_points, `[[`, character(1), "id")
  group_ids <- vapply(decision_groups, `[[`, character(1), "id")

  duplicate_leaf_ids <- unique(leaf_ids[duplicated(leaf_ids)])
  if (length(duplicate_leaf_ids) > 0L) {
    stop(
      prefix,
      sprintf(
        "duplicated DecisionPoint id(s) in `schema$decision_points`: %s. Each decision point must have a unique id.",
        paste(duplicate_leaf_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  duplicate_group_ids <- unique(group_ids[duplicated(group_ids)])
  if (length(duplicate_group_ids) > 0L) {
    stop(
      prefix,
      sprintf(
        "duplicated GroupedDecisionPoint id(s) in `schema$decision_groups`: %s.",
        paste(duplicate_group_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  shared_ids <- intersect(leaf_ids, group_ids)
  if (length(shared_ids) > 0L) {
    stop(
      prefix,
      sprintf(
        "DecisionPoint and GroupedDecisionPoint ids must be globally unique; shared id(s): %s.",
        paste(shared_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (i in seq_along(decision_groups)) {
    group <- decision_groups[[i]]
    nested_ids <- intersect(group$members, group_ids)
    if (length(nested_ids) > 0L) {
      stop(
        prefix,
        sprintf(
          "GroupedDecisionPoint '%s' references grouped id(s) {%s}; nested groups are not supported.",
          group$id, paste(nested_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    unknown_ids <- setdiff(group$members, leaf_ids)
    if (length(unknown_ids) > 0L) {
      stop(
        prefix,
        sprintf(
          "GroupedDecisionPoint '%s' references unknown DecisionPoint member id(s): %s.",
          group$id, paste(unknown_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  null_trigger_ids <- leaf_ids[vapply(decision_points, function(dp) is.null(dp$trigger), logical(1))]
  referenced_ids <- if (length(decision_groups) == 0L) {
    character()
  } else {
    unique(unlist(lapply(decision_groups, `[[`, "members"), use.names = FALSE))
  }
  unreferenced_ids <- setdiff(null_trigger_ids, referenced_ids)
  if (length(unreferenced_ids) > 0L) {
    stop(
      prefix,
      sprintf(
        "DecisionPoint id(s) with `trigger = NULL` must be referenced by at least one GroupedDecisionPoint: %s.",
        paste(unreferenced_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

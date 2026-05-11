# ------------------------------------------------------------------------------
# update_block()
#
# Purpose:
#   Validate and expand a block-based update payload into a named list suitable
#   for a single atomic entity state update (one event index j, many variables).
#
# Usage:
#   - Intended for use inside ModelBundle$transition(entity, event)
#   - 'values' must be named (vector or list); mixed types are supported.
#
# Key behaviors:
#   - Validates that supplied variables exist in the entity schema
#   - Validates block membership (variables must belong to the requested block)
#   - require_all=TRUE requires full block coverage; FALSE allows partial updates
#   - unknown controls how variables not in schema are handled
#
# Returns:
#   A named list of updates, ordered by schema/block variable order.
# ------------------------------------------------------------------------------

#' Block-oriented state updates for vectorized models
#'
#' Many models generate multivariate outputs (e.g., SBP/DBP or lab panels).
#' update_block() validates a named payload against an entity's schema and a
#' named schema block, then returns a named list of per-variable state updates for
#' use as a transition() return value.
#'
#' @param entity A Entity object.
#' @param block Block name (character scalar) corresponding to schema$<var>$blocks.
#' @param values Named vector or named list of values to update. Names must correspond
#' to state variables in the entity's schema.
#' @param require_all Logical; if TRUE (default) all variables belonging to block
#' must be supplied in values.
#' @param unknown Policy for variables supplied in values that are not present in
#' the entity's schema. One of "error" (default), "drop_warn_once", or
#' "drop_warn_always".
#'
#' @return
#' A named list of state updates suitable for returning from transition(). Values
#' are returned in schema block order.
#'
#' @export
update_block <- function(entity,
                         block,
                         values,
                         require_all = TRUE,
                         unknown = c("error", "drop_warn_once", "drop_warn_always")) {
  unknown <- match.arg(unknown)
  if (is.null(entity) || is.null(entity$schema)) stop("entity must have a $schema")
  schema <- entity$schema
  if (!is.list(schema) || is.null(names(schema))) stop("entity$schema must be a named list")
  if (!is.character(block) || length(block) != 1L || block == "") stop("block must be a non-empty string")
  if (!isTRUE(require_all) && !identical(require_all, FALSE)) stop("require_all must be TRUE or FALSE")

  vars_block <- block_vars(schema, block)
  if (length(vars_block) == 0L) stop(sprintf("Unknown or empty schema block '%s'.", block))

  # Normalize values to a named list
  if (is.null(values)) stop("values must be a named vector or named list")
  if (is.atomic(values) && !is.list(values)) {
    if (is.null(names(values)) || any(names(values) == "")) stop("values must be *named*")
    values_list <- as.list(values)
  } else if (is.list(values)) {
    if (is.null(names(values)) || any(names(values) == "")) stop("values must be a *named* list")
    values_list <- values
  } else {
    stop("values must be a named vector or named list")
  }

  supplied <- names(values_list)

  # Handle variables not in schema
  unknown_vars <- setdiff(supplied, names(schema))
  if (length(unknown_vars) > 0L) {
    msg <- sprintf(
      "update_block('%s') received variables not in schema and will drop them: %s",
      block, paste(unknown_vars, collapse = ", ")
    )
    if (unknown == "error") {
      stop(msg)
    } else {
      if (unknown == "drop_warn_always") {
        warning(msg, call. = FALSE)
      } else {
        .warn_once(paste0("update_block:", block, ":unknown"), msg)
      }
      values_list <- values_list[setdiff(supplied, unknown_vars)]
      supplied <- names(values_list)
    }
  }

  # Disallow updates to vars outside the block (even if they exist in schema)
  not_in_block <- setdiff(supplied, vars_block)
  if (length(not_in_block) > 0L) {
    stop(sprintf(
      "update_block('%s') received variables not in that block: %s",
      block, paste(not_in_block, collapse = ", ")
    ))
  }

  if (isTRUE(require_all)) {
    missing <- setdiff(vars_block, supplied)
    if (length(missing) > 0L) {
      stop(sprintf(
        "update_block('%s') is missing required variables: %s",
        block, paste(missing, collapse = ", ")
      ))
    }
    out <- values_list[vars_block]
  } else {
    out_vars <- intersect(vars_block, supplied)
    out <- values_list[out_vars]
  }

  # Ensure output is a named list (and in block/schema order)
  if (!is.list(out)) out <- as.list(out)
  out
}


# ------------------------------------------------------------------------------
# combine_updates()
#
# Purpose:
#   Combine multiple update payloads (named lists) into a single update payload.
#   This is useful when one event triggers updates from multiple sub-models.
#
# Key behaviors:
#   - NULL inputs are ignored
#   - Errors if the same variable is updated more than once in the same event
#
# Returns:
#   A single named list of updates.
# ------------------------------------------------------------------------------

#' Combine multiple update lists into one atomic update payload
#'
#' Convenience helper for transition() implementations that need to apply
#' multiple update sources (e.g., scalar tweaks plus one or more block updates).
#' The function concatenates named lists and errors if any variable is updated more
#' than once within the same event.
#'
#' @param ... One or more named lists of updates (or NULL).
#'
#' @return A single named list of updates, or NULL if all inputs are NULL.
#'
#' @export
combine_updates <- function(...) {
  parts <- list(...)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(NULL)

  for (i in seq_along(parts)) {
    x <- parts[[i]]
    if (!is.list(x)) stop("All update parts must be named lists or NULL")
    if (is.null(names(x)) || any(names(x) == "")) stop("All update parts must be *named* lists")
  }

  out <- do.call(c, parts)
  dups <- unique(names(out)[duplicated(names(out))])
  if (length(dups) > 0L) {
    stop(sprintf(
      "Duplicate updates within a single event are not allowed: %s",
      paste(dups, collapse = ", ")
    ))
  }
  out
}

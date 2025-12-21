#' Expand vectorized model outputs into per-variable state updates
#'
#' Many models generate multivariate predictions (e.g., SBP/DBP, CBC panels) but
#' `Patient$update()` expects a named list of scalar changes. These helpers
#' convert vectors into the expected named-list format.
#'
#' @name vector_helpers
NULL

#' Create a named list of updates from variable names and values
#'
#' @param vars Character vector of variable names.
#' @param values Vector of values (same length as `vars`).
#' @return Named list suitable for `transition()` returns.
#' @export
set_vars <- function(vars, values) {
  if (!is.character(vars) || length(vars) == 0L) stop("vars must be a non-empty character vector")
  if (length(values) != length(vars)) stop("values must have the same length as vars")
  as.list(stats::setNames(values, vars))
}

#' Create a named list of updates from a named vector
#'
#' @param x Named vector (e.g., numeric) of updates.
#' @param vars Optional character vector specifying which names to take and in what order.
#'   If provided, values are taken as `x[vars]` and names are set to `vars`.
#' @return Named list suitable for `transition()` returns.
#' @export
set_vars_from_named <- function(x, vars = NULL) {
  if (is.null(names(x))) stop("x must be a named vector")
  if (!is.null(vars)) {
    if (!is.character(vars) || length(vars) == 0L) stop("vars must be a non-empty character vector when provided")
    x <- x[vars]
    names(x) <- vars
  }
  as.list(x)
}

NULL

set_vars <- function(vars, values) {
  if (!is.character(vars) || length(vars) == 0L) stop("vars must be a non-empty character vector")
  if (length(values) != length(vars)) stop("values must have the same length as vars")
  as.list(stats::setNames(values, vars))
}

set_vars_from_named <- function(x, vars = NULL) {
  if (is.null(names(x))) stop("x must be a named vector")
  if (!is.null(vars)) {
    if (!is.character(vars) || length(vars) == 0L) stop("vars must be a non-empty character vector when provided")
    x <- x[vars]
    names(x) <- vars
  }
  as.list(x)
}

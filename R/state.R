#' Patient state object
#'
#' Internal helper class used for returning patient state without coercing
#' mixed types (e.g., numeric + character + Date) into a single atomic vector.
#'
#' `Patient$state()` returns an object of class `ps_state`.
#'
#' - `x[["var"]]` and `x$var` return the underlying value.
#' - For convenience/backward-compatibility, `x["var"]` (single index)
#'   returns the underlying value (like `[[`), not a one-element list.
#'
#' @keywords internal
NULL

.as_ps_state <- function(x) {
  if (is.null(x)) return(structure(list(), class = "ps_state"))
  stopifnot(is.list(x))
  structure(x, class = "ps_state")
}

#' @export
`[.ps_state` <- function(x, i, ...) {
  if (missing(i)) return(x)
  # single index: behave like [[ for backward-compatibility with older
  # vector-based state() returns (where x["var"] produced the value).
  if (length(i) == 1L) return(x[[i]])
  # multiple indices: keep list subset semantics
  x[i]
}

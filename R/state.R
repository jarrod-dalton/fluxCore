NULL

.as_state <- function(x) {
  if (is.null(x)) return(structure(list(), class = "ps_state"))
  stopifnot(is.list(x))
  structure(x, class = "ps_state")
}

`[.ps_state` <- function(x, i, ...) {
  if (missing(i)) return(x)
  # single index: behave like [[ for backward-compatibility with older
  # vector-based state() returns (where x["var"] produced the value).
  if (length(i) == 1L) return(x[[i]])
  # multiple indices: keep list subset semantics
  x[i]
}

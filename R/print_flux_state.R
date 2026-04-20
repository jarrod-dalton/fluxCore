print.flux_state <- function(x, ...) {
  # Lightweight print method for flux_state objects (internal state snapshots).
  # Keep it stable and readable for debugging and test output.
  cat("<flux_state>\n")
  base::print(unclass(x), ...)
  invisible(x)
}

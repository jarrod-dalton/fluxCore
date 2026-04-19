print.ps_state <- function(x, ...) {
  # Lightweight print method for ps_state objects (internal state snapshots).
  # Keep it stable and readable for debugging and test output.
  cat("<ps_state>\n")
  base::print(unclass(x), ...)
  invisible(x)
}

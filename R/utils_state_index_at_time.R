## Utility: map a continuous time `t` to the most recent discrete event index `j`.
##
## The engine and Patient class use a discrete event counter `j` (0 = baseline,
## 1..N = events). This helper returns the latest `j` with event time <= `t`.
##
## - If there are no events yet, returns 0.
## - If all event times are > t, returns 0.
## - If events include a `j` column, we use it; otherwise we fall back to row index.
state_index_at_time <- function(events, t) {
  if (!is.numeric(t) || length(t) != 1L || is.na(t)) {
    stop("t must be a single non-missing numeric", call. = FALSE)
  }

  if (is.null(events) || NROW(events) == 0L) {
    return(0L)
  }
  if (is.null(events$time)) {
    stop("events must include a 'time' column", call. = FALSE)
  }

  idx <- which(events$time <= t)
  if (length(idx) == 0L) {
    return(0L)
  }

  if (!is.null(events$j)) {
    return(as.integer(max(events$j[idx], na.rm = TRUE)))
  }
  return(as.integer(max(idx)))
}

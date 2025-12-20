#' Feature target: event type
#'
#' Create a target describing events of a given type in `patient$events`.
#'
#' @param event_type Character scalar.
#' @return A target list used by [derive()].
#' @export
event <- function(event_type) {
  if (length(event_type) != 1) stop("event_type must be length 1")
  list(kind = "event", event_type = as.character(event_type))
}

#' Feature target: variable history
#'
#' Create a target describing sparse history for a variable tracked in `patient$hist`.
#'
#' @param name Character scalar variable name.
#' @return A target list used by [derive()] and [lag_of()].
#' @export
var <- function(name) {
  if (length(name) != 1) stop("name must be length 1")
  list(kind = "var", name = as.character(name))
}

.get_window_idx <- function(patient, t, j, lookback_t = NULL, lookback_j = NULL, include_current = TRUE, clock = "time") {
  ev <- patient$events
  if (!clock %in% names(ev)) stop(sprintf("clock '%s' not found in patient$events", clock))
  tt <- ev[[clock]]
  jj <- ev$j

  if (!is.null(lookback_t)) {
    start <- t - as.numeric(lookback_t)
    if (include_current) {
      keep <- (tt > start) & (tt <= t)
    } else {
      keep <- (tt > start) & (tt < t)
    }
    keep <- keep & (jj <= j)
    return(keep)
  }

  if (!is.null(lookback_j)) {
    lookback_j <- as.integer(lookback_j)
    if (!is.finite(lookback_j) || lookback_j < 1L) stop("lookback_j must be a positive integer")
    j_end <- if (include_current) j else (j - 1L)
    j_start <- j_end - lookback_j
    keep <- (jj > j_start) & (jj <= j_end)
    return(keep)
  }

  if (include_current) {
    keep <- (jj <= j) & (tt <= t)
  } else {
    keep <- (jj <= (j - 1L)) & (tt < t)
  }
  keep
}

.empty_value <- function(fn, force, na_value) {
  if (!force) return(NULL)
  if (is.null(na_value)) na_value <- NA
  if (identical(fn, "count")) return(0L)
  na_value
}

#' Define a derived feature
#'
#' `derive()` returns a function computing a history-derived feature as of `(j, t)`.
#' Intended to populate `Patient$derived_vars` and be used by `Patient$snapshot*()`.
#'
#' @param name Name of the derived feature.
#' @param target A target created by [event()] or [var()].
#' @param lookback_t Optional numeric lookback window on the time axis. Takes precedence over `lookback_j`.
#' @param lookback_j Optional integer lookback window in event-index units.
#' @param fn Summary function name. For `event()` targets, only `count`, `any`, and `all` are supported.
#' @param include_current Logical; include the boundary event at `t`/`j` (default TRUE).
#' @param force Logical; if TRUE, return a value when there is no history (default FALSE).
#' @param na_value Value to return when `force=TRUE` and no history exists (default `NA`).
#' @param clock Which column in `patient$events` defines the time axis for `lookback_t` (default `"time"`).
#' @return A function `(patient, j, t) -> scalar or NULL`.
#' @export
derive <- function(name,
                   target,
                   lookback_t = NULL,
                   lookback_j = NULL,
                   fn = c("count", "min", "max", "mean", "median", "sd", "iqr", "any", "all"),
                   include_current = TRUE,
                   force = FALSE,
                   na_value = NA,
                   clock = "time") {
  fn <- match.arg(fn)
  if (length(name) != 1) stop("name must be length 1")
  if (!is.list(target) || is.null(target$kind)) stop("target must be created by event() or var()")

  function(patient, j = patient$j, t = patient$last_time) {
    j <- as.integer(j); t <- as.numeric(t)
    keep <- .get_window_idx(patient, t = t, j = j,
                            lookback_t = lookback_t, lookback_j = lookback_j,
                            include_current = include_current, clock = clock)

    if (identical(target$kind, "event")) {
      et <- target$event_type
      idx <- keep & (patient$events$event_type == et)
      n <- sum(idx)
      if (n == 0L) return(.empty_value(fn, force, na_value))
      if (identical(fn, "count")) return(n)
      if (identical(fn, "any")) return(TRUE)
      if (identical(fn, "all")) return(sum(keep) == n)
      stop("For event() targets, only fn in {count, any, all} is supported.")
    }

    if (identical(target$kind, "var")) {
      vname <- target$name
      h <- patient$hist[[vname]]
      if (is.null(h)) return(.empty_value(fn, force, na_value))
      jj <- h$j; vv <- h$v
      in_j <- jj <= j
      if (!any(in_j)) return(.empty_value(fn, force, na_value))
      jj2 <- jj[in_j]
      vv2 <- vv[in_j]

      if (!is.null(lookback_t)) {
        ev_time <- patient$events[[clock]][match(jj2, patient$events$j)]
        start <- t - as.numeric(lookback_t)
        in_t <- if (include_current) (ev_time > start) & (ev_time <= t) else (ev_time > start) & (ev_time < t)
        vv2 <- vv2[in_t]
      } else if (!is.null(lookback_j)) {
        j_end <- if (include_current) j else (j - 1L)
        j_start <- j_end - as.integer(lookback_j)
        vv2 <- vv2[(jj2 > j_start) & (jj2 <= j_end)]
      } else {
        ev_time <- patient$events[[clock]][match(jj2, patient$events$j)]
        vv2 <- if (include_current) vv2[ev_time <= t] else vv2[ev_time < t]
      }

      if (length(vv2) == 0) return(.empty_value(fn, force, na_value))

      if (identical(fn, "count")) return(length(vv2))
      if (identical(fn, "min")) return(min(vv2))
      if (identical(fn, "max")) return(max(vv2))
      if (identical(fn, "mean")) return(mean(vv2))
      if (identical(fn, "median")) return(stats::median(vv2))
      if (identical(fn, "sd")) return(stats::sd(vv2))
      if (identical(fn, "iqr")) return(stats::IQR(vv2))
      if (identical(fn, "any")) return(any(vv2))
      if (identical(fn, "all")) return(all(vv2))
      stop("Unsupported fn.")
    }

    stop("Unknown target kind.")
  }
}

#' Define a lagged feature from variable history
#'
#' Extract the k-th most recent value of a variable from sparse history as of `(j, t)`.
#'
#' @param name Name of the derived feature.
#' @param target A target created by [var()].
#' @param k Positive integer lag order (1 = most recent prior value by default).
#' @param lookback_t Optional numeric lookback window on the time axis. Takes precedence over `lookback_j`.
#' @param lookback_j Optional integer lookback window in event-index units.
#' @param include_current Logical; include boundary event at `t`/`j` (default FALSE).
#' @param force Logical; if TRUE return `na_value` when insufficient history exists (default FALSE).
#' @param na_value Value to return when `force=TRUE` and insufficient history exists (default `NA`).
#' @param clock Which column in `patient$events` defines the time axis for `lookback_t` (default `"time"`).
#' @return A function `(patient, j, t) -> scalar or NULL`.
#' @export
lag_of <- function(name,
                   target,
                   k = 1,
                   lookback_t = NULL,
                   lookback_j = NULL,
                   include_current = FALSE,
                   force = FALSE,
                   na_value = NA,
                   clock = "time") {
  if (length(name) != 1) stop("name must be length 1")
  if (!is.list(target) || !identical(target$kind, "var")) stop("target must be var(<name>)")
  k <- as.integer(k)
  if (!is.finite(k) || k < 1L) stop("k must be a positive integer")

  function(patient, j = patient$j, t = patient$last_time) {
    j <- as.integer(j); t <- as.numeric(t)
    vname <- target$name
    h <- patient$hist[[vname]]
    if (is.null(h)) return(if (force) na_value else NULL)

    jj <- h$j; vv <- h$v
    in_j <- jj <= j
    jj2 <- jj[in_j]
    vv2 <- vv[in_j]
    if (length(vv2) == 0) return(if (force) na_value else NULL)

    if (!is.null(lookback_t)) {
      ev_time <- patient$events[[clock]][match(jj2, patient$events$j)]
      start <- t - as.numeric(lookback_t)
      in_t <- if (include_current) (ev_time > start) & (ev_time <= t) else (ev_time > start) & (ev_time < t)
      vv2 <- vv2[in_t]
    } else if (!is.null(lookback_j)) {
      j_end <- if (include_current) j else (j - 1L)
      j_start <- j_end - as.integer(lookback_j)
      vv2 <- vv2[(jj2 > j_start) & (jj2 <= j_end)]
    } else {
      ev_time <- patient$events[[clock]][match(jj2, patient$events$j)]
      vv2 <- if (include_current) vv2[ev_time <= t] else vv2[ev_time < t]
    }

    if (length(vv2) < k) return(if (force) na_value else NULL)
    vv2[[length(vv2) - k + 1L]]
  }
}

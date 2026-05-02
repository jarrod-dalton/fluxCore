# Time axis helpers -----------------------------------------------------------
#
# The flux ecosystem operates on a numeric model time axis.
# These helpers provide deterministic mappings between calendar time
# (Date / POSIXct) and numeric model time under a declared time spec.
#
# Key concepts:
# - time_spec(unit=..., origin=..., zone=...) is the canonical declaration.
# - ctx$time remains a compatibility transport path.
# - 'months' and 'years' are fixed approximations (30.4375 and 365.25 days).
#
# NOTE: time_origin is NOT baseline. Baseline/start-of-followup is model-defined.

.time_allowed_units <- c(
  "seconds", "minutes", "hours", "days", "weeks", "months", "years"
)

.time_days_per_unit <- function(unit) {
  unit <- tolower(unit)
  switch(
    unit,
    seconds = 1 / 86400,
    minutes = 60 / 86400,
    hours   = 3600 / 86400,
    days    = 1,
    weeks   = 7,
    months  = 30.4375,
    years   = 365.25,
    stop("Unsupported time unit: ", unit, call. = FALSE)
  )
}

#' Compile and validate canonical time settings
#'
#' Compiles and validates calendar-to-model time settings from explicit arguments (preferred) or ctx$time (compatibility).
#'
#' @param unit Required time unit when using explicit arguments. One of "seconds", "minutes", "hours", "days", "weeks", "months", "years".
#' @param origin Optional Date or POSIXct/POSIXt origin used for calendar-time conversion. Defaults to Unix epoch.
#' @param zone Time zone used for calendar-time conversion (default "UTC").
#' @param ctx Optional compatibility path. A context list containing ctx$time$unit and optional ctx$time$origin/ctx$time$zone.
#'
#' @return An object of class time_spec with precomputed conversion constants.
#'
#' @export
time_spec <- function(unit = NULL, origin = NULL, zone = "UTC", ctx = NULL) {
  # Backward-compatibility path: time_spec(ctx)
  if (is.list(unit) && is.null(ctx) && is.null(origin) && identical(zone, "UTC")) {
    ctx <- unit
    unit <- NULL
  }

  if (!is.null(ctx)) {
    if (!is.list(ctx)) stop("ctx must be a list.", call. = FALSE)
    if (is.null(ctx$time) || !is.list(ctx$time)) {
      stop("ctx$time must be a list with fields unit/origin/zone.", call. = FALSE)
    }
    if (!is.null(unit)) {
      stop("Specify either `unit`/`origin`/`zone` or `ctx`, not both.", call. = FALSE)
    }
    unit <- ctx$time$unit
    origin <- ctx$time$origin
    if (!is.null(ctx$time$zone)) zone <- ctx$time$zone
  }

  if (is.null(unit) || !is.character(unit) || length(unit) != 1L || !nzchar(unit)) {
    stop("unit must be a non-empty single string.", call. = FALSE)
  }
  unit <- tolower(unit)
  if (!unit %in% .time_allowed_units) {
    stop(
      "Unsupported unit: '", unit, "'. Allowed: ",
      paste(.time_allowed_units, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(zone) || length(zone) != 1L || !nzchar(zone)) {
    stop("zone must be a non-empty single string (e.g., 'UTC').", call. = FALSE)
  }
  zone <- as.character(zone)

  # Validate time zone once (expensive-ish), not in per-call conversion.
  if (!identical(zone, "UTC") && !(zone %in% OlsonNames())) {
    stop(
      "Invalid zone: '", zone, "'. Use an IANA/Olson time zone name (e.g., 'UTC', 'America/New_York').",
      call. = FALSE
    )
  }

  if (is.null(origin)) {
    # Default origin: system epoch. Use POSIXct (UTC) as canonical internal origin,
    # and also keep a Date origin for Date arithmetic.
    origin <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  }

  origin_class <- NULL
  if (inherits(origin, "Date")) {
    origin_class <- "Date"
  } else if (inherits(origin, c("POSIXct", "POSIXt"))) {
    origin_class <- "POSIXct"
  } else {
    stop("origin must be a Date or POSIXct/POSIXt.", call. = FALSE)
  }

  # Canonical internal origin for POSIX arithmetic, stored in ctx$time$zone.
  origin_posix <- as.POSIXct(origin, tz = zone)
  origin_date  <- as.Date(origin_posix, tz = zone)

  days_per_unit <- .time_days_per_unit(unit)
  seconds_per_unit <- days_per_unit * 86400

  structure(
    list(
      unit = unit,
      zone = zone,
      origin = origin,           # as provided (Date or POSIXct)
      origin_class = origin_class,
      origin_posix = origin_posix,
      origin_date = origin_date,
      days_per_unit = days_per_unit,
      seconds_per_unit = seconds_per_unit
    ),
    class = "time_spec"
  )
}

#' Convert calendar time to numeric model time
#'
#' Converts numeric, Date, or POSIXct/POSIXt time to numeric model time under a compiled time spec.
#'
#' @param x A numeric vector, Date, or POSIXct/POSIXt vector of times.
#' @param time_spec A compiled time spec from time_spec(unit = ...).
#'
#' @return Numeric model time in units of time_spec$unit.
#'
#' @export
time_to_model <- function(x, time_spec) {
  if (is.null(time_spec) || !inherits(time_spec, "time_spec")) {
    stop("time_spec must be a 'time_spec' created by time_spec(...).", call. = FALSE)
  }

  # Explicitly disallow "time-only" classes (no date component). These are
  # common in data pipelines but cannot be mapped to model time without a date.
  # Examples: difftime, hms.
  if (inherits(x, "difftime") || inherits(x, "hms")) {
    stop(
      "Time-only inputs are not supported. Provide Date or POSIXct (date+time), or numeric model time.",
      call. = FALSE
    )
  }

  if (is.numeric(x)) {
    if (anyNA(x) || any(!is.finite(x))) stop("Numeric time contains NA/Inf.", call. = FALSE)
    return(as.numeric(x))
  }

  if (inherits(x, "Date")) {
    diff_days <- as.numeric(x - time_spec$origin_date)
    if (anyNA(diff_days) || any(!is.finite(diff_days))) stop("Date time contains NA/Inf.", call. = FALSE)
    return(diff_days / time_spec$days_per_unit)
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    xx <- as.POSIXct(x, tz = time_spec$zone)
    diff_secs <- as.numeric(difftime(xx, time_spec$origin_posix, units = "secs"))
    if (anyNA(diff_secs) || any(!is.finite(diff_secs))) stop("POSIXct time contains NA/Inf.", call. = FALSE)
    return(diff_secs / time_spec$seconds_per_unit)
  }

  stop("x must be numeric, Date, or POSIXct/POSIXt.", call. = FALSE)
}

#' Convert numeric model time to calendar time
#'
#' Converts numeric model time to Date or POSIXct using a compiled time spec.
#'
#' @param t Numeric model time.
#' @param time_spec A compiled time spec from time_spec(unit = ...).
#' @param class Output class: 'origin' (match the origin class), 'Date', or 'POSIXct'.
#'
#' @return A Date or POSIXct vector.
#'
#' @export
time_from_model <- function(t, time_spec, class = c("origin", "Date", "POSIXct")) {
  if (is.null(time_spec) || !inherits(time_spec, "time_spec")) {
    stop("time_spec must be a 'time_spec' created by time_spec(...).", call. = FALSE)
  }
  class <- match.arg(class)

  if (!is.numeric(t)) stop("t must be numeric.", call. = FALSE)
  if (anyNA(t) || any(!is.finite(t))) stop("Numeric model time contains NA/Inf.", call. = FALSE)

  secs <- t * time_spec$seconds_per_unit
  out_posix <- time_spec$origin_posix + secs

  want_posix <- identical(class, "POSIXct") || (identical(class, "origin") && identical(time_spec$origin_class, "POSIXct"))
  if (want_posix) {
    return(as.POSIXct(out_posix, tz = time_spec$zone))
  }
  return(as.Date(out_posix, tz = time_spec$zone))
}

#' Set time settings in ctx
#'
#' Convenience helper to set ctx$time$unit and optionally ctx$time$origin and ctx$time$zone.
#'
#' @param ctx Context list (or NULL to create a new one).
#' @param unit Time unit string. Must be one of: seconds, minutes, hours, days, weeks, months, years.
#' @param origin Optional Date or POSIXct origin used as a mapping reference. Default is system epoch.
#' @param zone Optional IANA/Olson time zone string. Default is 'UTC'.
#'
#' @return Updated ctx list (validated).
#'
#' @export
set_time_unit <- function(ctx = NULL, unit, origin = NULL, zone = "UTC") {
  if (is.null(ctx)) ctx <- list()
  if (!is.list(ctx)) stop("ctx must be a list.", call. = FALSE)
  if (is.null(ctx$time) || !is.list(ctx$time)) ctx$time <- list()

  ctx$time$unit <- unit
  if (!is.null(origin)) ctx$time$origin <- origin
  if (!is.null(zone)) ctx$time$zone <- zone

  # Validate once; then return.
  time_spec(ctx = ctx)
  ctx
}

.time_ctx_from_spec <- function(spec) {
  if (is.null(spec) || !inherits(spec, "time_spec")) {
    stop("spec must be a 'time_spec'.", call. = FALSE)
  }
  list(
    unit = spec$unit,
    origin = spec$origin,
    zone = spec$zone
  )
}

.time_spec_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  if (!inherits(a, "time_spec") || !inherits(b, "time_spec")) return(FALSE)
  identical(a$unit, b$unit) &&
    identical(a$zone, b$zone) &&
    identical(a$origin_class, b$origin_class) &&
    isTRUE(all.equal(as.numeric(a$origin_posix), as.numeric(b$origin_posix), tolerance = 0))
}

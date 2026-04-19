# Time axis helpers -----------------------------------------------------------
#
# The patientSim ecosystem operates on a numeric model time axis.
# These helpers provide deterministic mappings between calendar time
# (Date / POSIXct) and numeric model time under a user-declared time unit.
#
# Key concepts:
# - ctx$time$unit is REQUIRED when mapping calendar time; it is a modeling choice.
# - ctx$time$origin is a reference used for conversion (default: system epoch).
# - ctx$time$zone defaults to 'UTC' and is validated against Olson/IANA tz names.
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

time_spec <- function(ctx) {
  if (is.null(ctx) || !is.list(ctx)) {
    stop("ctx must be a list.", call. = FALSE)
  }
  if (is.null(ctx$time) || !is.list(ctx$time)) {
    stop("ctx$time must be a list with fields unit/origin/zone.", call. = FALSE)
  }

  unit <- ctx$time$unit
  if (is.null(unit) || !is.character(unit) || length(unit) != 1L || !nzchar(unit)) {
    stop("ctx$time$unit must be a non-empty single string.", call. = FALSE)
  }
  unit <- tolower(unit)
  if (!unit %in% .time_allowed_units) {
    stop(
      "Unsupported ctx$time$unit: '", unit, "'. Allowed: ",
      paste(.time_allowed_units, collapse = ", "),
      call. = FALSE
    )
  }

  zone <- ctx$time$zone
  if (is.null(zone)) zone <- "UTC"
  if (!is.character(zone) || length(zone) != 1L || !nzchar(zone)) {
    stop("ctx$time$zone must be a non-empty single string (e.g., 'UTC').", call. = FALSE)
  }
  zone <- as.character(zone)

  # Validate time zone once (expensive-ish), not in per-call conversion.
  if (!identical(zone, "UTC") && !(zone %in% OlsonNames())) {
    stop(
      "Invalid ctx$time$zone: '", zone, "'. Use an IANA/Olson time zone name (e.g., 'UTC', 'America/New_York').",
      call. = FALSE
    )
  }

  origin <- ctx$time$origin
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
    stop("ctx$time$origin must be a Date or POSIXct/POSIXt.", call. = FALSE)
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

time_to_model <- function(x, time_spec) {
  if (is.null(time_spec) || !inherits(time_spec, "time_spec")) {
    stop("time_spec must be a 'time_spec' created by time_spec(ctx).", call. = FALSE)
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

time_from_model <- function(t, time_spec, class = c("origin", "Date", "POSIXct")) {
  if (is.null(time_spec) || !inherits(time_spec, "time_spec")) {
    stop("time_spec must be a 'time_spec' created by time_spec(ctx).", call. = FALSE)
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

set_time_unit <- function(ctx = NULL, unit, origin = NULL, zone = "UTC") {
  if (is.null(ctx)) ctx <- list()
  if (!is.list(ctx)) stop("ctx must be a list.", call. = FALSE)
  if (is.null(ctx$time) || !is.list(ctx$time)) ctx$time <- list()

  ctx$time$unit <- unit
  if (!is.null(origin)) ctx$time$origin <- origin
  if (!is.null(zone)) ctx$time$zone <- zone

  # Validate once; then return.
  time_spec(ctx)
  ctx
}

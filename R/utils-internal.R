.validate_event_time <- function(time, last_time) {
  time <- as.numeric(time)
  if (!is.finite(time) || length(time) != 1L) stop("time must be a finite numeric scalar.")
  if (!is.null(last_time) && time < as.numeric(last_time)) {
    stop(sprintf("Event time must be non-decreasing (got time=%g < last_time=%g).", time, last_time))
  }
  time
}

.validate_event_type <- function(event_type) {
  if (length(event_type) != 1L) stop("event_type must be a length-1 character value.")
  event_type <- as.character(event_type)
  if (is.na(event_type) || nchar(event_type) == 0) stop("event_type must be non-empty.")
  event_type
}

.validate_schema <- function(schema) {
  if (!is.list(schema) || is.null(names(schema)) || any(names(schema) == "")) {
    stop("schema must be a named list.")
  }
  for (k in names(schema)) {
    spec <- schema[[k]]
    if (!is.list(spec)) stop(sprintf("schema[['%s']] must be a list.", k))
    if (is.null(spec$default)) stop(sprintf("schema[['%s']] must define $default.", k))
    # coerce is optional; default is identity
    if (is.null(spec$coerce)) {
      spec$coerce <- function(x) x
    }
    if (!is.function(spec$coerce)) {
      stop(sprintf("schema[['%s']] $coerce must be a function or NULL.", k))
    }
    if (!is.null(spec$validate) && !is.function(spec$validate)) {
      stop(sprintf("schema[['%s']] $validate must be a function or NULL.", k))
    }

    # Declared variable type metadata (used by downstream summary/validation code).
    if (is.null(spec$type)) {
      stop(sprintf("schema[['%s']] must define $type.", k))
    }
    t <- tolower(as.character(spec$type)[1])
    ok <- c("binary","categorical","ordinal","continuous","count")
    if (!(t %in% ok)) {
      stop(sprintf("schema[['%s']] $type must be one of: %s", k, paste(ok, collapse = ", ")))
    }
    spec$type <- t

    # Support / level metadata
    if (t %in% c("binary","categorical","ordinal")) {
      if (is.null(spec$levels)) {
        stop(sprintf("schema[['%s']] must define $levels (character, length>=2) for type='%s'.", k, t))
      }
      if (!is.character(spec$levels) || any(is.na(spec$levels)) || any(spec$levels == "") || length(spec$levels) < 2L) {
        stop(sprintf("schema[['%s']] $levels must be a non-empty character vector (length>=2).", k))
      }
      if (length(unique(spec$levels)) != length(spec$levels)) {
        stop(sprintf("schema[['%s']] $levels must not contain duplicates.", k))
      }
    } else {
      # count/continuous: levels may be provided (e.g., for display bins), but are optional
      if (!is.null(spec$levels)) {
        if (!is.character(spec$levels) || any(is.na(spec$levels)) || any(spec$levels == "")) {
          stop(sprintf("schema[['%s']] $levels must be a character vector with no empty values.", k))
        }
      }
    }


  }
  invisible(TRUE)
}

.init_state_from_schema <- function(schema, init) {
  init <- if (is.null(init)) list() else init
  if (!is.list(init)) stop("init must be a list or NULL.")
  if (!is.null(names(init)) && any(names(init) == "")) stop("init must be a *named* list or NULL.")

  state <- vector("list", length(schema))
  names(state) <- names(schema)

  for (k in names(schema)) {
    spec <- schema[[k]]
    val <- if (!is.null(names(init)) && k %in% names(init)) init[[k]] else spec$default
    val <- spec$coerce(val)

    if (!is.null(spec$validate) && !isTRUE(spec$validate(val))) {
      stop(sprintf("Invalid initial value for '%s'.", k))
    }
    state[[k]] <- val
  }

  if (!is.null(names(init))) {
    extras <- setdiff(names(init), names(schema))
    if (length(extras) > 0) stop(sprintf("init contained unknown state vars: %s", paste(extras, collapse = ", ")))
  }

  state
}

.init_hist_from_state <- function(state) {
  lapply(state, function(v) list(j = 0L, v = v))
}

.apply_changes <- function(current, hist, schema, j, changes) {
  if (is.null(changes) || length(changes) == 0L) {
    return(list(current = current, hist = hist))
  }
  if (!is.list(changes)) stop("changes must be a named list or NULL.")
  nms <- names(changes)
  if (is.null(nms) || any(nms == "")) stop("changes must be a *named* list or NULL.")

  extras <- setdiff(nms, names(schema))
  if (length(extras) > 0) stop(sprintf("changes contained unknown state vars: %s", paste(extras, collapse = ", ")))

  for (k in nms) {
    spec <- schema[[k]]
    val <- spec$coerce(changes[[k]])

    if (!is.null(spec$validate) && !isTRUE(spec$validate(val))) {
      stop(sprintf("Invalid update for '%s' at j=%d.", k, j))
    }

    current[[k]] <- val
    hist[[k]]$j <- c(hist[[k]]$j, j)
    hist[[k]]$v <- c(hist[[k]]$v, val)
  }

  list(current = current, hist = hist)
}

.validate_model_bundle <- function(bundle) {
  if (!is.list(bundle)) stop("bundle must be a list.")
  required <- c("propose_events", "transition", "stop")
  missing <- setdiff(required, names(bundle))
  if (length(missing) > 0) stop(sprintf("ModelBundle is missing: %s", paste(missing, collapse = ", ")))

  if (!is.function(bundle$propose_events)) stop("bundle$propose_events must be a function.")
  if (!is.function(bundle$transition)) stop("bundle$transition must be a function.")
  if (!is.function(bundle$stop)) stop("bundle$stop must be a function.")

  if (!is.null(bundle$observe) && !is.function(bundle$observe)) stop("bundle$observe must be a function or NULL.")
  if (!is.null(bundle$refresh_rules) && !is.function(bundle$refresh_rules)) stop("bundle$refresh_rules must be a function or NULL.")
  # Optional: bundle$params can provide default parameters for a run.
  if (!is.null(bundle$params) && !is.list(bundle$params)) stop("bundle$params must be a list or NULL.")

  invisible(TRUE)
}


.psim_internal_env <- new.env(parent = emptyenv())

.warn_once <- function(key, msg) {
  if (!is.character(key) || length(key) != 1L) key <- "__default__"
  if (isTRUE(.psim_internal_env[[key]])) return(invisible(FALSE))
  .psim_internal_env[[key]] <- TRUE
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

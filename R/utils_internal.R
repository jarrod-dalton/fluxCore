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
    # coerce is optional; default based on type
    if (!is.null(spec$coerce) && !is.function(spec$coerce)) {
      stop(sprintf("schema[['%s']] $coerce must be a function or NULL.", k))
    }
    if (!is.null(spec$validate) && !is.function(spec$validate)) {
      stop(sprintf("schema[['%s']] $validate must be a function or NULL.", k))
    }

    if (is.null(spec$required)) {
      spec$required <- FALSE
    } else if (!is.logical(spec$required) || length(spec$required) != 1L || is.na(spec$required)) {
      stop(sprintf("schema[['%s']] $required must be a single TRUE/FALSE value.", k))
    }

    if (is.null(spec$allow_na)) {
      spec$allow_na <- FALSE
    } else if (!is.logical(spec$allow_na) || length(spec$allow_na) != 1L || is.na(spec$allow_na)) {
      stop(sprintf("schema[['%s']] $allow_na must be a single TRUE/FALSE value.", k))
    }

    if (!is.null(spec$min)) {
      if (!is.numeric(spec$min) || length(spec$min) != 1L || is.na(spec$min)) {
        stop(sprintf("schema[['%s']] $min must be a single finite numeric value.", k))
      }
    }
    if (!is.null(spec$max)) {
      if (!is.numeric(spec$max) || length(spec$max) != 1L || is.na(spec$max)) {
        stop(sprintf("schema[['%s']] $max must be a single finite numeric value.", k))
      }
    }
    if (!is.null(spec$min) && !is.null(spec$max) && spec$max < spec$min) {
      stop(sprintf("schema[['%s']] $max must be greater than or equal to $min.", k))
    }

    # Declared variable type metadata (used by downstream summary/validation code).
    if (is.null(spec$type)) {
      stop(sprintf("schema[['%s']] must define $type.", k))
    }
    t <- tolower(as.character(spec$type)[1])
    # Alias "continuous" to "numeric" for backward compatibility
    if (t == "continuous") t <- "numeric"
    ok <- c("logical", "binary", "integer", "count", "nonnegative_integer", "positive_integer",
            "numeric", "nonnegative_numeric", "positive_numeric", "probability", "percent",
            "categorical", "ordinal", "string", "nonempty_string")
    if (!(t %in% ok)) {
      stop(sprintf("schema[['%s']] $type must be one of: %s", k, paste(ok, collapse = ", ")))
    }
    spec$type <- t

    # Set default coerce based on type if not provided
    if (is.null(spec$coerce)) {
      spec$coerce <- switch(t,
        logical = as.logical,
        binary = as.logical,
        integer = as.integer,
        count = as.integer,
        nonnegative_integer = as.integer,
        positive_integer = as.integer,
        numeric = as.numeric,
        nonnegative_numeric = as.numeric,
        positive_numeric = as.numeric,
        probability = as.numeric,
        percent = as.numeric,
        categorical = as.character,
        ordinal = as.character,
        string = as.character,
        nonempty_string = as.character,
        function(x) x  # fallback
      )
    }

    # Set default value based on type if not provided
    if (is.null(spec$default)) {
      spec$default <- switch(t,
        logical = NA,
        binary = NA,
        integer = NA_integer_,
        count = NA_integer_,
        nonnegative_integer = NA_integer_,
        positive_integer = NA_integer_,
        numeric = NA_real_,
        nonnegative_numeric = NA_real_,
        positive_numeric = NA_real_,
        probability = NA_real_,
        percent = NA_real_,
        categorical = NA_character_,
        ordinal = NA_character_,
        string = NA_character_,
        nonempty_string = NA_character_,
        NA  # fallback
      )
    }

    if (!is.null(spec$min) || !is.null(spec$max)) {
      numeric_types <- c("integer", "count", "nonnegative_integer", "positive_integer",
                         "numeric", "nonnegative_numeric", "positive_numeric", "probability", "percent")
      if (!t %in% numeric_types) {
        stop(sprintf("schema[['%s']] $min/$max are only supported for numeric types.", k))
      }
    }

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
      # Other types: levels may be provided (e.g., for display bins), but are optional
      if (!is.null(spec$levels)) {
        if (!is.character(spec$levels) || any(is.na(spec$levels)) || any(spec$levels == "")) {
          stop(sprintf("schema[['%s']] $levels must be a character vector with no empty values.", k))
        }
      }
    }


    schema[[k]] <- spec
  }
  schema
}

.validate_state_value <- function(spec, val, var_name, phase = "value") {
  if (length(val) != 1L) {
    stop(sprintf("Value for '%s' must be a scalar.", var_name), call. = FALSE)
  }

  if (is.na(val)) {
    if (!isTRUE(spec$allow_na)) {
      stop(sprintf("Value for '%s' must not be missing.", var_name), call. = FALSE)
    }
  } else {
    t <- spec$type
    if (t == "logical") {
      if (!is.logical(val)) {
        stop(sprintf("Value for '%s' must be logical.", var_name), call. = FALSE)
      }
    } else if (t == "binary") {
      if (!is.logical(val) && !(is.numeric(val) && val %in% c(0, 1)) && !as.character(val) %in% spec$levels) {
        stop(
          sprintf(
            "Value for '%s' must be logical or one of: %s",
            var_name,
            paste(spec$levels, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    } else if (t %in% c("categorical", "ordinal")) {
      if (!as.character(val) %in% spec$levels) {
        stop(
          sprintf(
            "Value for '%s' must be one of: %s",
            var_name,
            paste(spec$levels, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    } else if (t == "integer") {
      if (!is.integer(val) && !(is.numeric(val) && val == as.integer(val))) {
        stop(sprintf("Value for '%s' must be an integer.", var_name), call. = FALSE)
      }
    } else if (t == "count") {
      if (!is.integer(val) && !(is.numeric(val) && val == as.integer(val)) || val < 0) {
        stop(sprintf("Value for '%s' must be a non-negative integer.", var_name), call. = FALSE)
      }
    } else if (t == "nonnegative_integer") {
      if (!is.integer(val) && !(is.numeric(val) && val == as.integer(val)) || val < 0) {
        stop(sprintf("Value for '%s' must be a non-negative integer.", var_name), call. = FALSE)
      }
    } else if (t == "positive_integer") {
      if (!is.integer(val) && !(is.numeric(val) && val == as.integer(val)) || val <= 0) {
        stop(sprintf("Value for '%s' must be a positive integer.", var_name), call. = FALSE)
      }
    } else if (t == "numeric") {
      if (!is.numeric(val)) {
        stop(sprintf("Value for '%s' must be numeric.", var_name), call. = FALSE)
      }
    } else if (t == "nonnegative_numeric") {
      if (!is.numeric(val) || val < 0) {
        stop(sprintf("Value for '%s' must be a non-negative numeric.", var_name), call. = FALSE)
      }
    } else if (t == "positive_numeric") {
      if (!is.numeric(val) || val <= 0) {
        stop(sprintf("Value for '%s' must be a positive numeric.", var_name), call. = FALSE)
      }
    } else if (t == "probability") {
      if (!is.numeric(val) || val < 0 || val > 1) {
        stop(sprintf("Value for '%s' must be a probability (0 <= x <= 1).", var_name), call. = FALSE)
      }
    } else if (t == "percent") {
      if (!is.numeric(val) || val < 0 || val > 100) {
        stop(sprintf("Value for '%s' must be a percent (0 <= x <= 100).", var_name), call. = FALSE)
      }
    } else if (t == "string") {
      if (!is.character(val)) {
        stop(sprintf("Value for '%s' must be a character string.", var_name), call. = FALSE)
      }
    } else if (t == "nonempty_string") {
      if (!is.character(val) || nchar(val) == 0) {
        stop(sprintf("Value for '%s' must be a non-empty character string.", var_name), call. = FALSE)
      }
    }
    
    # Additional min/max validation for numeric types
    if (!is.null(spec$min) || !is.null(spec$max)) {
      if (!is.numeric(val)) {
        stop(sprintf("Value for '%s' must be numeric for min/max validation.", var_name), call. = FALSE)
      }
      if (!is.null(spec$min) && val < spec$min) {
        stop(sprintf("Value for '%s' must be >= %s.", var_name, spec$min), call. = FALSE)
      }
      if (!is.null(spec$max) && val > spec$max) {
        stop(sprintf("Value for '%s' must be <= %s.", var_name, spec$max), call. = FALSE)
      }
    }
  }

  if (!is.null(spec$validate) && !isTRUE(spec$validate(val))) {
    stop(sprintf("Invalid %s value for '%s'.", phase, var_name), call. = FALSE)
  }

  invisible(TRUE)
}

.init_state_from_schema <- function(schema, init) {
  init <- if (is.null(init)) list() else init
  if (!is.list(init)) stop("init must be a list or NULL.")
  if (!is.null(names(init))) {
    if (any(names(init) == "")) stop("init must be a *named* list or NULL.")
    if (anyDuplicated(names(init))) stop("init must not contain duplicate names.")
  }

  state <- vector("list", length(schema))
  names(state) <- names(schema)

  for (k in names(schema)) {
    spec <- schema[[k]]
    has_init <- !is.null(names(init)) && k %in% names(init)
    if (!has_init && isTRUE(spec$required)) {
      stop(sprintf("Missing required init state var '%s'.", k), call. = FALSE)
    }

    val <- if (has_init) init[[k]] else spec$default
    val <- spec$coerce(val)
    .validate_state_value(spec, val, k, phase = "initial")
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
  if (anyDuplicated(nms)) stop("changes must not contain duplicate names.")

  extras <- setdiff(nms, names(schema))
  if (length(extras) > 0) stop(sprintf("changes contained unknown state vars: %s", paste(extras, collapse = ", ")))

  for (k in nms) {
    spec <- schema[[k]]
    val <- spec$coerce(changes[[k]])
    .validate_state_value(spec, val, k, phase = "update")

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
  if (is.null(bundle$time_spec) || !inherits(bundle$time_spec, "time_spec")) {
    stop("ModelBundle must define `$time_spec` as a fluxCore `time_spec` object.", call. = FALSE)
  }

  .validate_bundle_event_metadata(bundle)

  invisible(TRUE)
}

.validate_bundle_event_set <- function(x, field_name) {
  if (is.null(x)) return(NULL)
  if (!is.character(x)) {
    stop(sprintf("bundle$%s must be a character vector when provided.", field_name), call. = FALSE)
  }
  x <- unique(as.character(x))
  if (length(x) < 1L || any(is.na(x)) || any(!nzchar(x))) {
    stop(sprintf("bundle$%s must contain one or more non-empty event labels.", field_name), call. = FALSE)
  }
  x
}

.validate_bundle_event_metadata <- function(bundle) {
  event_catalog <- .validate_bundle_event_set(bundle$event_catalog, "event_catalog")
  terminal_events <- .validate_bundle_event_set(bundle$terminal_events, "terminal_events")

  if (!is.null(event_catalog) && !is.null(terminal_events)) {
    unknown <- setdiff(terminal_events, event_catalog)
    if (length(unknown) > 0L) {
      stop(
        sprintf(
          "bundle$terminal_events contains labels not in bundle$event_catalog: %s",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.ctx_time_spec <- function(ctx) {
  if (is.null(ctx) || !is.list(ctx)) return(NULL)
  if (!is.null(ctx$time_spec) && inherits(ctx$time_spec, "time_spec")) return(ctx$time_spec)
  if (!is.null(ctx$time) && is.list(ctx$time)) {
    out <- tryCatch(time_spec(ctx = list(time = ctx$time)), error = function(e) NULL)
    return(out)
  }
  NULL
}

.assert_ctx_time_compatible <- function(ctx, canonical_time_spec, where = "ctx") {
  if (is.null(ctx) || !is.list(ctx)) return(invisible(TRUE))
  if (is.null(canonical_time_spec) || !inherits(canonical_time_spec, "time_spec")) {
    stop("canonical_time_spec must be a fluxCore `time_spec`.", call. = FALSE)
  }
  supplied <- .ctx_time_spec(ctx)
  if (!is.null(supplied) && !.time_spec_equal(supplied, canonical_time_spec)) {
    stop(
      sprintf(
        "%s attempted to override canonical model time spec. Declare model time once in the bundle and do not override at runtime.",
        where
      ),
      call. = FALSE
    )
  }
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

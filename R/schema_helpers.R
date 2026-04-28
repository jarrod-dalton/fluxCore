# Schema helper utilities -----------------------------------------------------
#
# The flux ecosystem treats the model schema as a binding contract.
# These helpers provide fast, centralized validation and lookup of schema
# metadata (variable existence, type, levels, blocks).

schema_validate <- function(schema) {
  invisible(.validate_schema(schema))
}

schema_assert_vars <- function(schema, vars) {
  schema <- .validate_schema(schema)
  if (is.null(vars) || length(vars) == 0L) return(invisible(TRUE))
  if (!is.character(vars) || anyNA(vars) || any(vars == "")) {
    stop("vars must be a character vector of non-empty names.", call. = FALSE)
  }
  missing <- setdiff(unique(vars), names(schema))
  if (length(missing) > 0L) {
    stop(sprintf("Unknown schema variable(s): %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

schema_var_info <- function(schema, vars) {
  schema <- .validate_schema(schema)
  if (!is.character(vars) || length(vars) == 0L) {
    stop("vars must be a non-empty character vector.", call. = FALSE)
  }
  schema_assert_vars(schema, vars)
  vars <- as.character(vars)

  types <- vapply(vars, function(v) schema[[v]]$type, character(1))
  levels <- lapply(vars, function(v) {
    lv <- schema[[v]]$levels
    if (is.null(lv)) return(NULL)
    lv
  })
  blocks <- lapply(vars, function(v) {
    b <- schema[[v]]$blocks
    if (is.null(b)) return(character())
    b
  })

  data.frame(
    var = vars,
    type = types,
    levels = I(levels),
    blocks = I(blocks),
    stringsAsFactors = FALSE
  )
}

schema_assert_types <- function(schema, vars, allowed_types) {
  schema <- .validate_schema(schema)
  schema_assert_vars(schema, vars)
  if (!is.character(allowed_types) || length(allowed_types) == 0L) {
    stop("allowed_types must be a non-empty character vector.", call. = FALSE)
  }
  allowed_types <- tolower(as.character(allowed_types))
  bad <- character()
  for (v in unique(vars)) {
    t <- tolower(as.character(schema[[v]]$type)[1])
    if (!t %in% allowed_types) bad <- c(bad, v)
  }
  if (length(bad) > 0L) {
    stop(
      sprintf(
        "Schema variable(s) have incompatible type (allowed: %s): %s",
        paste(allowed_types, collapse = ", "),
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

schema_assert_levels <- function(schema, var, levels) {
  schema <- .validate_schema(schema)
  if (!is.character(var) || length(var) != 1L || is.na(var) || var == "") {
    stop("var must be a single non-empty string.", call. = FALSE)
  }
  schema_assert_vars(schema, var)
  if (!is.character(levels) || length(levels) == 0L || anyNA(levels) || any(levels == "")) {
    stop("levels must be a non-empty character vector with no empty values.", call. = FALSE)
  }

  t <- tolower(as.character(schema[[var]]$type)[1])
  if (!t %in% c("binary", "categorical", "ordinal")) {
    stop(sprintf("Variable '%s' is type='%s' and does not declare categorical levels.", var, t), call. = FALSE)
  }

  declared <- schema[[var]]$levels
  extra <- setdiff(unique(levels), declared)
  if (length(extra) > 0L) {
    stop(
      sprintf(
        "Variable '%s' has level(s) not declared in schema: %s",
        var,
        paste(extra, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

schema_validator_numeric <- function(min = -Inf, max = Inf, allow_na = FALSE) {
  if (!is.numeric(min) || length(min) != 1L || is.na(min)) {
    stop("min must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(max) || length(max) != 1L || is.na(max)) {
    stop("max must be a single numeric value.", call. = FALSE)
  }
  if (max < min) {
    stop("max must be greater than or equal to min.", call. = FALSE)
  }
  if (!is.logical(allow_na) || length(allow_na) != 1L || is.na(allow_na)) {
    stop("allow_na must be a single TRUE/FALSE value.", call. = FALSE)
  }

  function(x) {
    if (length(x) != 1L) return(FALSE)
    if (is.na(x)) return(isTRUE(allow_na))
    is.numeric(x) && is.finite(x) && x >= min && x <= max
  }
}

schema_validator_integer <- function(min = -Inf, max = Inf, allow_na = FALSE) {
  if (!is.numeric(min) || length(min) != 1L || is.na(min)) {
    stop("min must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(max) || length(max) != 1L || is.na(max)) {
    stop("max must be a single numeric value.", call. = FALSE)
  }
  if (max < min) {
    stop("max must be greater than or equal to min.", call. = FALSE)
  }
  if (!is.logical(allow_na) || length(allow_na) != 1L || is.na(allow_na)) {
    stop("allow_na must be a single TRUE/FALSE value.", call. = FALSE)
  }

  function(x) {
    if (length(x) != 1L) return(FALSE)
    if (is.na(x)) return(isTRUE(allow_na))
    if (!is.numeric(x) || !is.finite(x) || x != floor(x)) return(FALSE)
    x >= min && x <= max
  }
}

schema_validator_levels <- function(levels, allow_na = FALSE) {
  if (!is.character(levels) || length(levels) == 0L || anyNA(levels) || any(levels == "")) {
    stop("levels must be a non-empty character vector with no missing or empty values.", call. = FALSE)
  }
  if (!is.logical(allow_na) || length(allow_na) != 1L || is.na(allow_na)) {
    stop("allow_na must be a single TRUE/FALSE value.", call. = FALSE)
  }
  levels <- unique(levels)

  function(x) {
    if (length(x) != 1L) return(FALSE)
    if (is.na(x)) return(isTRUE(allow_na))
    as.character(x) %in% levels
  }
}

set_schema <- function(vars = NULL, schema = NULL, replace = FALSE, add = TRUE, remove = NULL) {
  # Validate inputs
  if (!is.null(vars)) {
    if (!is.character(vars) || is.null(names(vars)) || any(names(vars) == "")) {
      stop("vars must be a named character vector.", call. = FALSE)
    }
  }
  if (!is.null(schema)) {
    if (!is.list(schema) || is.null(names(schema)) || any(names(schema) == "")) {
      stop("schema must be a named list.", call. = FALSE)
    }
  }
  if (!is.logical(replace) || length(replace) != 1L || is.na(replace)) {
    stop("replace must be a single TRUE/FALSE value.", call. = FALSE)
  }
  if (!is.logical(add) || length(add) != 1L || is.na(add)) {
    stop("add must be a single TRUE/FALSE value.", call. = FALSE)
  }
  if (!is.null(remove)) {
    if (!is.character(remove) || anyNA(remove) || any(remove == "")) {
      stop("remove must be a character vector with no missing or empty values.", call. = FALSE)
    }
  }

  # Handle replace
  if (replace) {
    if (is.null(schema)) {
      stop("schema must be supplied when replace=TRUE.", call. = FALSE)
    }
    if (!is.null(vars)) {
      schema <- list()
    } else {
      stop("vars must be supplied when replace=TRUE.", call. = FALSE)
    }
  } else {
    if (is.null(schema)) {
      schema <- list()
    }
  }

  # Handle remove
  if (!is.null(remove)) {
    if (is.null(schema)) {
      stop("schema must be supplied when remove is specified.", call. = FALSE)
    }
    for (var in remove) {
      if (!var %in% names(schema)) {
        warning(sprintf("Variable '%s' not found in schema, skipping removal.", var), call. = FALSE)
      } else {
        schema[[var]] <- NULL
      }
    }
  }

  # Handle add
  if (add && !is.null(vars)) {
    for (var in names(vars)) {
      if (var %in% names(schema)) {
        stop(sprintf("Variable '%s' already exists in schema.", var), call. = FALSE)
      }
      type <- vars[[var]]
      spec <- list(type = type)
      # Add type-specific defaults if needed
      if (type %in% c("binary", "categorical", "ordinal")) {
        stop(sprintf("Type '%s' requires levels to be specified separately.", type), call. = FALSE)
      }
      schema[[var]] <- spec
    }
  }

  # Validate the final schema
  schema <- .validate_schema(schema)
  schema
}

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

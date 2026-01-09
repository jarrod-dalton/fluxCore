# Schema helper utilities -----------------------------------------------------
#
# The patientSim ecosystem treats the model schema as a binding contract.
# These helpers provide fast, centralized validation and lookup of schema
# metadata (variable existence, type, levels, blocks).

#' Validate a patient schema
#'
#' Validates the structure and metadata requirements of a patient schema.
#' The schema is a named list where each entry describes a state variable.
#'
#' @param schema A named list schema.
#'
#' @return The validated schema (invisibly), with normalized type fields.
#' @export
ps_schema_validate <- function(schema) {
  invisible(.validate_schema(schema))
}

#' Assert that variables exist in a schema
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param vars Character vector of variable names.
#'
#' @return Invisibly TRUE.
#' @export
ps_schema_assert_vars <- function(schema, vars) {
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

#' Get schema metadata for variables
#'
#' Returns a data.frame with one row per requested variable and columns:
#' `var`, `type`, `levels`, and `blocks`.
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param vars Character vector of variable names.
#'
#' @return A data.frame.
#' @export
ps_schema_var_info <- function(schema, vars) {
  schema <- .validate_schema(schema)
  if (!is.character(vars) || length(vars) == 0L) {
    stop("vars must be a non-empty character vector.", call. = FALSE)
  }
  ps_schema_assert_vars(schema, vars)
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

#' Assert that schema variables have allowed types
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param vars Character vector of variable names.
#' @param allowed_types Character vector of allowed schema types.
#'
#' @return Invisibly TRUE.
#' @export
ps_schema_assert_types <- function(schema, vars, allowed_types) {
  schema <- .validate_schema(schema)
  ps_schema_assert_vars(schema, vars)
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

#' Assert that categorical/ordinal/binary variables use declared levels
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param var A single variable name.
#' @param levels Character vector of levels to check.
#'
#' @return Invisibly TRUE.
#' @export
ps_schema_assert_levels <- function(schema, var, levels) {
  schema <- .validate_schema(schema)
  if (!is.character(var) || length(var) != 1L || is.na(var) || var == "") {
    stop("var must be a single non-empty string.", call. = FALSE)
  }
  ps_schema_assert_vars(schema, var)
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

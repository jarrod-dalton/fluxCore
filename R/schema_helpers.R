# Schema helper utilities -----------------------------------------------------
#
# The flux ecosystem treats the model schema as a binding contract.
# These helpers provide fast, centralized validation and lookup of schema
# metadata (variable existence, type, levels, blocks).

#' Validate an entity schema
#'
#' Validates the structure and required metadata of an entity schema.
#' The schema is a named list where each entry describes one state variable.
#'
#' @param schema A named list schema.
#'
#' @return The validated schema (invisibly), with normalized type fields.
#'
#' @export
schema_validate <- function(schema) {
  invisible(.validate_schema(schema))
}

#' Assert that variables exist in a schema
#'
#' Stops with an informative error if any requested variable names are not present in the schema.
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param vars Character vector of variable names.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
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

#' Get schema metadata for variables
#'
#' Returns schema metadata (type, levels, blocks) for the requested variables.
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param vars Character vector of variable names.
#'
#' @return A data.frame with columns var, type, levels, and blocks.
#'
#' @export
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

#' Assert that variables have allowed schema types
#'
#' Stops with an informative error if any variables have schema types outside the allowed set.
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param vars Character vector of variable names.
#' @param allowed_types Character vector of allowed schema types.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
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

#' Assert that levels are declared in the schema
#'
#' For binary/categorical/ordinal variables, stops if any provided levels are not declared in the schema.
#'
#' @param schema A validated schema (or a raw schema that can be validated).
#' @param var A single variable name.
#' @param levels Character vector of levels to check.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
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

#' Create a numeric schema validator
#'
#' Returns a predicate function that checks a scalar numeric value against inclusive numeric bounds and optional missingness.
#'
#' @param min A single numeric lower bound.
#' @param max A single numeric upper bound.
#' @param allow_na Logical scalar indicating whether missing values are permitted.
#'
#' @return A function of signature function(x) -> TRUE/FALSE.
#'
#' @export
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

#' Create an integer schema validator
#'
#' Returns a predicate function that checks a scalar integer value against inclusive numeric bounds and optional missingness.
#'
#' @param min A single numeric lower bound.
#' @param max A single numeric upper bound.
#' @param allow_na Logical scalar indicating whether missing values are permitted.
#'
#' @return A function of signature function(x) -> TRUE/FALSE.
#'
#' @export
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

#' Create a levels-based schema validator
#'
#' Returns a predicate function that checks a scalar value against a set of allowed character levels and optional missingness.
#'
#' @param levels A character vector of allowed level labels.
#' @param allow_na Logical scalar indicating whether missing values are permitted.
#'
#' @return A function of signature function(x) -> TRUE/FALSE.
#'
#' @export
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

#' Build or extend a fluxCore schema with hybrid shorthand
#'
#' Constructs a validated fluxCore schema from a hybrid `vars` specification.
#' Each element of `vars` may be either a type-name string (e.g. `"count"`) or
#' a fully-specified list (e.g. `list(type = "positive_numeric", max = 20)`).
#' Optionally merges onto an existing `schema`, with explicit `overwrite` and
#' `remove` controls.
#'
#' When `time_spec` or `decision_points` is supplied, `set_schema()` returns a
#' **full schema list** (with `$variables`, `$time_spec`, and
#' `$decision_points`) suitable for direct use with [load_model()]. When
#' neither is supplied, it returns just the validated variables spec (backward-
#' compatible with prior usage and with `Entity$new(schema = ...)`).
#'
#' @param vars Named list (or character vector) of variable specs. Each element
#'   is either a single type-name string or a list containing `type` plus any
#'   recognized schema fields (`min`, `max`, `levels`, `default`, `coerce`,
#'   `validate`, `allow_na`, `required`, `blocks`).
#' @param schema Optional existing schema to extend. If `NULL`, a new schema is
#'   created. May be either a plain variables list or a full schema list (with a
#'   `$variables` field) — both forms are accepted.
#' @param remove Optional character vector of variable names to remove from
#'   `schema` before merging `vars`. Errors if any name is not present.
#' @param overwrite Logical scalar. If `FALSE` (default), adding a variable
#'   already present in `schema` is an error. If `TRUE`, existing entries are
#'   replaced.
#' @param time_spec Optional [time_spec()] object. When provided, the returned
#'   value is a full schema list with `$variables`, `$time_spec`, and
#'   `$decision_points`, ready to pass directly to [load_model()].
#' @param decision_points Optional list of [DecisionPoint()] objects to attach
#'   to the schema. Requires `time_spec` to also be provided. When supplied, the
#'   returned value is a full schema list (see `time_spec` above).
#'
#' @return A validated fluxCore variables spec (named list), or — when
#'   `time_spec` or `decision_points` is supplied — a full schema list with
#'   `$variables`, `$time_spec`, and `$decision_points`.
#'
#' @examples
#'   # Variables only (backward-compatible):
#'   vars <- set_schema(vars = list(
#'     route_zone  = list(type = "categorical",
#'                        levels = c("urban", "suburban", "rural")),
#'     battery_pct = "percent",
#'     payload_kg  = list(type = "positive_numeric", max = 20),
#'     deliveries  = "count",
#'     prob_rain   = "probability"
#'   ))
#'
#'   # Full schema for load_model():
#'   dp <- DecisionPoint(id = "dp1", trigger = "event_A",
#'                       allowed_actions = c("accept", "decline"))
#'   schema <- set_schema(
#'     vars             = list(battery_pct = "percent"),
#'     time_spec        = time_spec(unit = "hours"),
#'     decision_points  = list(dp)
#'   )
#'   # schema$variables, schema$time_spec, schema$decision_points are all set.
#'
#' @export
set_schema <- function(vars            = NULL,
                       schema          = NULL,
                       remove          = NULL,
                       overwrite       = FALSE,
                       time_spec       = NULL,
                       decision_points = NULL) {
  # decision_points requires time_spec
  if (!is.null(decision_points) && is.null(time_spec)) {
    stop("set_schema(): `time_spec` is required when `decision_points` is supplied.", call. = FALSE)
  }

  # Validate time_spec
  if (!is.null(time_spec) && !inherits(time_spec, "time_spec")) {
    stop("set_schema(): `time_spec` must be a time_spec object (from time_spec()).", call. = FALSE)
  }

  # Validate decision_points
  if (!is.null(decision_points)) {
    if (!is.list(decision_points) || length(decision_points) == 0L) {
      stop("set_schema(): `decision_points` must be a non-empty list of DecisionPoint objects.", call. = FALSE)
    }
    for (i in seq_along(decision_points)) {
      if (!inherits(decision_points[[i]], "DecisionPoint")) {
        stop(
          sprintf("set_schema(): `decision_points[[%d]]` is not a DecisionPoint object.", i),
          call. = FALSE
        )
      }
    }
  }

  # Accept a full schema list (with $variables) as the `schema` argument.
  if (!is.null(schema) && is.list(schema) && !is.null(schema$variables)) {
    schema <- schema$variables
  }

  if (!is.null(schema)) {
    if (!is.list(schema) || is.null(names(schema)) || any(names(schema) == "")) {
      stop("schema must be a named list.", call. = FALSE)
    }
  } else {
    schema <- list()
  }

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be a single TRUE/FALSE value.", call. = FALSE)
  }

  if (!is.null(remove)) {
    if (!is.character(remove) || anyNA(remove) || any(remove == "")) {
      stop("remove must be a character vector with no missing or empty values.", call. = FALSE)
    }
    missing <- setdiff(remove, names(schema))
    if (length(missing) > 0L) {
      stop(
        sprintf("remove names not found in schema: %s", paste(missing, collapse = ", ")),
        call. = FALSE
      )
    }
    for (nm in remove) schema[[nm]] <- NULL
  }

  if (is.null(vars)) {
    vars_schema <- .validate_schema(schema)
    if (!is.null(time_spec) || !is.null(decision_points)) {
      return(list(
        variables       = vars_schema,
        time_spec       = time_spec,
        decision_points = decision_points
      ))
    }
    return(vars_schema)
  }

  # Accept either a named character vector (legacy/shorthand) or a named list
  # whose elements may be strings or full spec lists (hybrid syntax).
  if (is.character(vars)) {
    vars <- as.list(vars)
  }
  if (!is.list(vars) || length(vars) == 0L ||
      is.null(names(vars)) || any(names(vars) == "") || anyNA(names(vars))) {
    stop("vars must be a non-empty named list (or named character vector).", call. = FALSE)
  }
  if (anyDuplicated(names(vars))) {
    stop("vars must not contain duplicate names.", call. = FALSE)
  }

  for (nm in names(vars)) {
    el <- vars[[nm]]
    if (is.character(el)) {
      if (length(el) != 1L || is.na(el) || nchar(el) == 0L) {
        stop(sprintf("vars[['%s']] string shorthand must be a single non-empty type name.", nm),
             call. = FALSE)
      }
      spec <- list(type = el)
    } else if (is.list(el)) {
      if (is.null(el$type)) {
        stop(sprintf("vars[['%s']] list spec must contain a `type` field.", nm), call. = FALSE)
      }
      spec <- el
    } else {
      stop(sprintf("vars[['%s']] must be a type-name string or a list spec.", nm), call. = FALSE)
    }

    if (nm %in% names(schema) && !isTRUE(overwrite)) {
      stop(
        sprintf("Variable '%s' already exists in schema (use overwrite = TRUE to replace).", nm),
        call. = FALSE
      )
    }
    schema[[nm]] <- spec
  }

  vars_schema <- .validate_schema(schema)

  # Return full schema list when time_spec or decision_points were supplied.
  if (!is.null(time_spec) || !is.null(decision_points)) {
    return(list(
      variables       = vars_schema,
      time_spec       = time_spec,
      decision_points = decision_points
    ))
  }

  vars_schema
}

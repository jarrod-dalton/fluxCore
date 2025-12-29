#' Add variables to a schema
#'
#' Convenience helper for model packages: add one or more variables (as SchemaVar
#' objects, or as lightweight lists with at least a `default` field) to an
#' existing flat schema.
#'
#' @param schema Named list. Existing schema.
#' @param vars Named list. Variables to add.
#' @param prefix Optional string prefix applied to each `vars` name.
#'
#' @return Updated schema (named list).
#' @export
schema_add <- function(schema, vars, prefix = "") {
  if (is.null(schema) || !is.list(schema) || is.null(names(schema))) {
    stop("schema must be a named list")
  }
  if (is.null(vars) || !is.list(vars) || is.null(names(vars))) {
    stop("vars must be a named list")
  }
  if (!is.character(prefix) || length(prefix) != 1L) {
    stop("prefix must be a single string")
  }

  new_names <- paste0(prefix, names(vars))
  if (anyDuplicated(new_names)) {
    stop("vars contains duplicated names after applying prefix")
  }
  collisions <- intersect(names(schema), new_names)
  if (length(collisions) > 0L) {
    stop("schema_add would overwrite existing variables: ", paste(collisions, collapse = ", "))
  }

  names(vars) <- new_names
  c(schema, vars)
}

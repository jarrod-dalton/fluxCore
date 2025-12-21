#' Default patient state schema
#'
#' A state schema defines the set of state attributes and how they should be handled.
#' Each entry is a list with components:
#'
#' - `default`: default value used when not supplied in `init`
#' - `coerce`:  function to coerce values (e.g., `as.numeric`)
#' - `validate`: optional function taking a value and returning TRUE/FALSE
#' - `blocks`: optional character vector of "schema blocks" this variable belongs to
#'   (e.g., `c("cbc", "cbc_diff")`). Variables may belong to multiple blocks.
#'
#' Extend/modify this schema to include dozens of attributes while keeping the `Patient`
#' implementation generic.
#'
#' @return A named list (schema) suitable for `Patient$new(schema = ...)`.
#'
#' @examples
#' library(patientSimCore)
#' schema <- default_patient_schema()
#' names(schema)
#' schema$age$default
#'
#' @export
default_patient_schema <- function() {
  list(
    age = list(
      default = 40,
      coerce = as.numeric,
      validate = function(x) length(x) == 1L && is.finite(x) && x >= 0
    ),
    miles_to_work = list(
      default = 10,
      coerce = as.numeric,
      validate = function(x) length(x) == 1L && is.finite(x) && x >= 0
    )
    # Add more attributes here.
  )
}


#' List unique schema blocks
#'
#' Extract the set of unique block names declared in a schema via the optional
#' `blocks` metadata field on each variable entry. Variables may belong to
#' multiple blocks (many-to-many).
#'
#' @param schema A patient state schema (named list).
#' @return Character vector of unique block names.
#' @export
schema_blocks <- function(schema) {
  if (is.null(schema) || !is.list(schema)) stop("schema must be a named list")
  blocks <- character()
  for (nm in names(schema)) {
    x <- schema[[nm]]
    b <- x$blocks
    if (is.null(b)) next
    if (!is.character(b)) stop("schema entry 'blocks' must be a character vector")
    blocks <- c(blocks, b)
  }
  unique(blocks)
}

#' Get variable names in a schema block
#'
#' Convenience helper to look up the variables belonging to a named schema block
#' (e.g., "bp", "cbc", "cbc_diff", "bmp", "cmp"). Membership is many-to-many:
#' a variable may appear in multiple blocks.
#'
#' @param schema A patient state schema (named list).
#' @param block Block name (character scalar).
#' @return Character vector of variable names in the order they appear in `schema`.
#' @export
block_vars <- function(schema, block) {
  if (is.null(schema) || !is.list(schema)) stop("schema must be a named list")
  if (!is.character(block) || length(block) != 1L || block == "") stop("block must be a non-empty string")

  out <- character()
  for (nm in names(schema)) {
    x <- schema[[nm]]
    b <- x$blocks
    if (is.null(b)) next
    if (!is.character(b)) stop("schema entry 'blocks' must be a character vector")
    if (block %in% b) out <- c(out, nm)
  }
  out
}

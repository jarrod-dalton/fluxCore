# ------------------------------------------------------------------------------
# schema_blocks()
#
# Purpose:
#   List all unique block names declared across variables in a schema.
# ------------------------------------------------------------------------------

#' List unique schema blocks
#'
#' Extract the set of unique block names declared in a schema via the optional
#' blocks metadata field on each variable entry. Variables may belong to
#' multiple blocks (many-to-many).
#'
#' @param schema An entity state schema (named list).
#'
#' @return Character vector of unique block names.
#'
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

# ------------------------------------------------------------------------------
# block_vars()
#
# Purpose:
#   Return variable names that belong to a given block, in schema order.
#
# Notes:
#   - Variables may belong to multiple blocks.
# ------------------------------------------------------------------------------

#' Get variable names in a schema block
#'
#' Convenience helper to look up the variables belonging to a named schema block
#' (e.g., "bp", "cbc", "cbc_diff", "bmp", "cmp").
#' Membership is many-to-many: a variable may appear in multiple blocks.
#'
#' @param schema An entity state schema (named list).
#' @param block Block name (character scalar).
#'
#' @return Character vector of variable names in the order they appear in schema.
#'
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

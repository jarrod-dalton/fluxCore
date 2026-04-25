# ------------------------------------------------------------------------------
# schema_blocks()
#
# Purpose:
#   List all unique block names declared across variables in a schema.
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# default_entity_schema()
#
# Purpose:
#   Define the core state schema for an Entity. The schema is a named list where
#   each entry describes one state variable (default, coercion, validation) and
#   optional metadata such as block membership via `blocks`.
#
# Notes:
#   - 'blocks' is an optional character vector. Variables may belong to multiple
#     blocks (many-to-many), e.g. sodium in both 'bmp' and 'cmp'.
# ------------------------------------------------------------------------------

default_entity_schema <- function() {
  list(
    # Canonical vital status indicator.
    alive = list(
      type     = "binary",
      levels   = c("0","1"),
      default  = TRUE,
      coerce   = as.logical,
      validate = function(x) length(x) == 1L && (is.na(x) || is.logical(x))
    ),

    # Indicates whether the entity is under active follow-up / in-scope (distinct from alive).
    #
    # IMPORTANT (v1.0 semantics): this is *just another state variable*.
    # - The Engine does not automatically stop when active_followup becomes FALSE.
    # - If you want follow-up to stop, implement that in your bundle's stop()
    #   logic (or via your own model-specific hooks) and/or use active_followup
    #   in Forecast eligibility predicates.
    active_followup = list(
      type     = "binary",
      levels   = c("0","1"),
      default  = TRUE,
      coerce   = as.logical,
      validate = function(x) length(x) == 1L && (is.na(x) || is.logical(x))
    )
  )
}


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

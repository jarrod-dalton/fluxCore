# ------------------------------------------------------------------------------
# default_patient_schema()
#
# Purpose:
#   Define the core state schema for a Patient. The schema is a named list where
#   each entry describes one state variable (default, coercion, validation) and
#   optional metadata such as block membership via `blocks`.
#
# Notes:
#   - 'blocks' is an optional character vector. Variables may belong to multiple
#     blocks (many-to-many), e.g. sodium in both 'bmp' and 'cmp'.
# ------------------------------------------------------------------------------

default_patient_schema <- function() {
  list(
    # Canonical vital status indicator.
    alive = list(
      default  = TRUE,
      coerce   = as.logical,
      validate = function(x) length(x) == 1L && !is.na(x)
    ),

    age = list(
      default = 40,
      coerce = as.numeric,
      validate = function(x) length(x) == 1L && is.finite(x) && x >= 0
    ),

    # Sex is often required by disease model packages.
    sex = list(
      default = "U",
      coerce = as.character,
      validate = function(x) length(x) == 1L && !is.na(x) && nzchar(x)
    )
  )
}


# ------------------------------------------------------------------------------
# model_active_schema_var()
#
# Purpose:
#   Helper to define an opt-in schema entry that can track which model scopes are
#   currently active along the canonical patient time axis.
#
# Details:
#   The value is a named logical vector, e.g. c(ascvd=TRUE, hospital=FALSE).
#   This is distinct from `alive` and from any model-specific follow-up flags.
#
# Usage:
#   schema <- c(default_patient_schema(), list(model_active = model_active_schema_var(c("ascvd","hospital"))))
# ------------------------------------------------------------------------------

model_active_schema_var <- function(scopes = "model", default = NULL) {
  scopes <- as.character(scopes)
  if (length(scopes) < 1L || any(is.na(scopes)) || any(scopes == "")) {
    stop("scopes must be a non-empty character vector")
  }
  default_val <- stats::setNames(rep(TRUE, length(scopes)), scopes)
  if (!is.null(default)) {
    if (!is.logical(default) || is.null(names(default))) {
      stop("default must be a named logical vector")
    }
    if (!all(names(default) %in% scopes)) {
      stop("default names must be a subset of scopes")
    }
    # Fill missing scopes with TRUE by default.
    filled <- default_val
    filled[names(default)] <- default
    default_val <- filled
  }
  list(
    default = default_val,
    coerce = function(x) x,
    validate = function(x) {
      is.logical(x) && !is.null(names(x)) && length(x) >= 1L &&
        all(names(x) != "") && all(!is.na(x))
    }
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

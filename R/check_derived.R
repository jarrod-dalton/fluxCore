# ------------------------------------------------------------------------------
# check_derived(entity, derived_vars, replace = FALSE)
#
# Register a named list of derived variable functions on an Entity.
#
# Derived variables are NOT core state. They are computed at snapshot time.
# In fluxCore, an entity's derived variables are stored as a named list
# of functions in entity$derived_vars.
#
# Contract for each derived var function:
#   f(entity, j, t) -> scalar (or NULL to omit)
#
# Rules:
#   - derived_vars must be a named list of functions
#   - names must be non-empty strings
#   - if a name already exists:
#       * replace = FALSE -> keep existing
#       * replace = TRUE  -> overwrite
#
# This function is intended to be called once per run during initialization
# (e.g., via bundle$init_entity()).
# ------------------------------------------------------------------------------
check_derived <- function(entity, derived_vars, replace = FALSE) {
  if (is.null(derived_vars)) return(invisible(entity))

  if (!inherits(entity, "Entity")) stop("entity must be an Entity.")
  if (!is.list(derived_vars)) stop("derived_vars must be a named list (or NULL).")

  nms <- names(derived_vars)
  if (is.null(nms) || any(nms == "")) stop("derived_vars must be a named list with non-empty names.")

  for (nm in nms) {
    f <- derived_vars[[nm]]
    if (!is.function(f)) stop(sprintf("derived_vars[['%s']] must be a function.", nm))

    exists <- !is.null(entity$derived_vars[[nm]])
    if (!exists || isTRUE(replace)) {
      entity$derived_vars[[nm]] <- f
    }
  }

  invisible(entity)
}

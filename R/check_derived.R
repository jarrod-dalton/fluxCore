# ------------------------------------------------------------------------------
# check_derived(patient, derived_vars, replace = FALSE)
#
# Register a named list of derived variable functions on a Patient.
#
# Derived variables are NOT core state. They are computed at snapshot time.
# In patientSimCore, a patient's derived variables are stored as a named list
# of functions in patient$derived_vars.
#
# Contract for each derived var function:
#   f(patient, j, t) -> scalar (or NULL to omit)
#
# Rules:
#   - derived_vars must be a named list of functions
#   - names must be non-empty strings
#   - if a name already exists:
#       * replace = FALSE -> keep existing
#       * replace = TRUE  -> overwrite
#
# This function is intended to be called once per run during initialization
# (e.g., via bundle$init_patient()).
# ------------------------------------------------------------------------------
check_derived <- function(patient, derived_vars, replace = FALSE) {
  if (is.null(derived_vars)) return(invisible(patient))

  if (!inherits(patient, "Patient")) stop("patient must be a Patient.")
  if (!is.list(derived_vars)) stop("derived_vars must be a named list (or NULL).")

  nms <- names(derived_vars)
  if (is.null(nms) || any(nms == "")) stop("derived_vars must be a named list with non-empty names.")

  for (nm in nms) {
    f <- derived_vars[[nm]]
    if (!is.function(f)) stop(sprintf("derived_vars[['%s']] must be a function.", nm))

    exists <- !is.null(patient$derived_vars[[nm]])
    if (!exists || isTRUE(replace)) {
      patient$derived_vars[[nm]] <- f
    }
  }

  invisible(patient)
}

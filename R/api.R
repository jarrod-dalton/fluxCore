# ------------------------------------------------------------------------------
# new_patient()
#
# Purpose:
#   Convenience constructor for a Patient R6 object. Designed to make it easy to
#   create a patient from "real patient" inputs (e.g., a 1-row data.frame).
#
# Parameters:
#   init        Initial state overrides. Supported:
#                - named list (preferred)
#                - named atomic vector
#                - 1-row data.frame
#                - NULL (treated as empty list)
#   schema      Patient schema (see default_patient_schema()).
#   time0       Initial event time (numeric).
#   event_type0 Event type label for the initial event.
#
# Returns:
#   A Patient R6 object.
# ------------------------------------------------------------------------------

new_patient <- function(init = list(),
                        schema = default_patient_schema(),
                        time0 = 0,
                        event_type0 = "init") {

  # Accept common "real patient" inputs:
  # - named list (preferred)
  # - named atomic vector
  # - 1-row data.frame
  if (is.data.frame(init)) {
    if (nrow(init) != 1L) stop("If init is a data.frame it must have exactly one row.", call. = FALSE)
    init <- as.list(init[1, , drop = FALSE])
  } else if (!is.list(init) && !is.null(init)) {
    if (is.atomic(init) && !is.null(names(init))) {
      init <- as.list(init)
    } else {
      stop("init must be a named list, a named atomic vector, a 1-row data.frame, or NULL.", call. = FALSE)
    }
  }

  if (is.null(init)) init <- list()

  Patient$new(init = init, schema = schema, time0 = time0, event_type0 = event_type0)
}

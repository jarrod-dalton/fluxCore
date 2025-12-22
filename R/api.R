# ------------------------------------------------------------------------------
# new_patient()
#
# Purpose:
#   Convenience constructor for a Patient R6 object.
#
# Parameters:
#   init        Named list of initial state values to override schema defaults.
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
  Patient$new(init = init, schema = schema, time0 = time0, event_type0 = event_type0)
}

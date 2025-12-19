#' Construct a new Patient
#'
#' Convenience constructor wrapping `Patient$new()`.
#'
#' @param init Named list of initial state values. Names must be in `schema`.
#' @param schema State schema (named list). See [default_patient_schema()].
#' @param time0 Initial event time (numeric scalar, default 0).
#' @param event_type0 Initial event type (character, default "init").
#' @return A `Patient` R6 object.
#'
#' @examples
#' library(patientSimCore)
#' p <- new_patient(init = list(age = 50, miles_to_work = 8))
#' p$state(c("age", "miles_to_work"))
#'
#' @export
new_patient <- function(init,
                        schema = default_patient_schema(),
                        time0 = 0,
                        event_type0 = "init") {
  Patient$new(init = init, schema = schema, time0 = time0, event_type0 = event_type0)
}

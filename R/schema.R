#' Default patient state schema
#'
#' A state schema defines the set of state attributes and how they should be handled.
#' Each entry is a list with components:
#'
#' - `default`: default value used when not supplied in `init`
#' - `coerce`:  function to coerce values (e.g., `as.numeric`)
#' - `validate`: optional function taking a value and returning TRUE/FALSE
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

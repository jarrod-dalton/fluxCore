## ModelBundle contract (concept)
##
## @name modelbundle-concept
##
## A ModelBundle is a named list of callables that define simulation dynamics.
## The Engine uses these functions to propose the next event, compute state updates,
## and decide when to stop.
##
## Required functions:
## - `propose_event(patient, ctx)` -> list(time_next, event_type, ...)
## - `transition(patient, event_type, time_next, ctx)` -> named list `changes` or NULL
## - `stop(patient, event_type, ctx)` -> TRUE/FALSE
##
## Optional functions:
## - `observe(patient, event_type, ctx)` -> list/data.frame row of observed outputs for logging
##
## A bundle can close over fitted models, parameters, reference data, etc. This makes
## it easy to swap dynamics while keeping the Patient and Engine generic.
##
## @keywords internal
NULL
#' Default toy ModelBundle
#'
#' Provides a small, self-contained bundle that demonstrates:
#' - non-uniform inter-event times
#' - event types including a terminal `"death"`
#' - sparse state updates via `changes` patches
#'
#' The toy model assumes your schema contains `age` and optionally `miles_to_work`.
#'
#' @param terminal_event_type Character scalar. Default `"death"`.
#' @return A ModelBundle (named list of functions).
#'
#' @examples
#' library(patientSimCore)
#' set.seed(1)
#' bundle <- default_model_bundle()
#' names(bundle)
#'
#' @export
default_model_bundle <- function(terminal_event_type = "death") {
  terminal_event_type <- as.character(terminal_event_type)

  propose_event <- function(patient, ctx = NULL) {
    if (!"age" %in% names(patient$current)) stop("Toy propose_event requires 'age' in patient state.")
    age <- as.numeric(patient$current$age)

    mean_dt <- max(0.05, 2.0 - 0.02 * (age - 40))
    dt <- stats::rexp(1, rate = 1 / mean_dt)
    time_next <- as.numeric(patient$last_time) + dt

    p_death <- plogis((age - 80) / 6)
    event_type <- if (stats::runif(1) < p_death) terminal_event_type else "visit"

    list(time_next = time_next, event_type = event_type, dt = dt)
  }

  transition <- function(patient, event_type, time_next, ctx = NULL) {
    if (identical(event_type, terminal_event_type)) return(NULL)

    dt <- time_next - as.numeric(patient$last_time)
    changes <- list(age = as.numeric(patient$current$age) + dt)

    if ("miles_to_work" %in% names(patient$current)) {
      new_miles <- as.numeric(patient$current$miles_to_work) + stats::rnorm(1, 0, 0.2)
      changes$miles_to_work <- max(0, new_miles)
    }

    changes
  }

  stop <- function(patient, event_type, ctx = NULL) {
    identical(event_type, terminal_event_type)
  }

  observe <- function(patient, event_type, ctx = NULL) {
    list(
      age = if ("age" %in% names(patient$current)) as.numeric(patient$current$age) else NA_real_,
      event_type = as.character(event_type)
    )
  }

  list(
    propose_event = propose_event,
    transition = transition,
    stop = stop,
    observe = observe,
    # Optional: sample global parameter draws. Default returns NULL draws.
    sample_params = function(D) rep(list(NULL), as.integer(D)),
    terminal_event_type = terminal_event_type
  )
}

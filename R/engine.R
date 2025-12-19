#' Engine: orchestrate patient simulation using a ModelProvider + model_spec
#'
#' The `Engine` runs simulations by orchestrating the interaction between:
#'
#' - a mutable `Patient` object (state + history + event log), and
#' - a `ModelBundle` (functions that propose events, apply transitions, and stop).
#'
#' ## Key design: Engine owns model materialization
#'
#' Instead of passing a bundle directly, you pass:
#' - a `provider` (e.g., [PackageProvider], [FileProvider], [MLflowProvider]), and
#' - a `model_spec` (a named list describing which model/version/artifacts to load).
#'
#' This keeps your simulation code stable as you evolve how models are stored, versioned,
#' and retrained.
#'
#' @section Bundle functions:
#' A ModelBundle is a named list containing:
#' - `propose_event(patient, ctx)` -> list(time_next, event_type, ...)
#' - `transition(patient, event_type, time_next, ctx)` -> named list `changes` or NULL
#' - `stop(patient, event_type, ctx)` -> logical scalar
#' - optional `observe(patient, event_type, ctx)` -> list/row; accumulated by Engine
#'
#' @field provider A ModelProvider (e.g., [PackageProvider], [FileProvider]).
#' @field model_spec Named list describing which model/bundle to load.
#' @field bundle The loaded ModelBundle (named list of functions).
#'
#' @param provider Provider object with a `$load(model_spec, ...)` method.
#' @param model_spec Named list describing which model/bundle to load.
#' @param ... Additional arguments passed to the provider when loading the model bundle.
#'
#' @examples
#' library(patientSimCore)
#' set.seed(1)
#'
#' prov <- PackageProvider$new()
#' eng <- Engine$new(provider = prov, model_spec = list(name = "default"))
#'
#' p <- Patient$new(init = list(age = 55, miles_to_work = 10), schema = default_patient_schema(), time0 = 0)
#' out <- eng$run(p, max_events = 200)
#' tail(out$events, 3)
#'
#' @export
Engine <- R6::R6Class(
  classname = "Engine",
  public = list(
    provider = NULL,
    model_spec = NULL,
    bundle = NULL,

    initialize = function(provider = PackageProvider$new(),
                          model_spec = list(name = "default"),
                          ...) {
      self$provider <- provider
      self$model_spec <- model_spec

      # provider must have a $load() method
      if (!("load" %in% names(provider)) && !is.function(provider$load)) {
        stop("provider must be an object with a $load(model_spec, ...) method.")
      }

      self$bundle <- provider$load(model_spec = model_spec, ...)
      .validate_model_bundle(self$bundle)

      invisible(self)
    },

    #' Run a simulation for one patient
    #'
    #' @param patient A `Patient` R6 object.
    #' @param max_events Maximum number of additional events to simulate.
    #' @param max_time Optional numeric; stop if patient time exceeds this value.
    #' @param return_observations Logical; if TRUE, accumulate `bundle$observe()` outputs.
    #' @param ctx Optional list passed through to bundle functions (e.g., draw_id, sim_id, params).
    #' @return A list with elements:
    #' - `patient`: the modified patient object
    #' - `events`: the patient's event log
    #' - `observations`: data.frame of observations (if requested and available)
    run = function(patient,
                   max_events = 1000,
                   max_time = NULL,
                   return_observations = TRUE,
                   ctx = NULL) {

      max_events <- as.integer(max_events)
      if (!is.finite(max_events) || max_events < 0L) stop("max_events must be a non-negative integer.")
      if (!is.null(max_time)) {
        max_time <- as.numeric(max_time)
        if (length(max_time) != 1L || !is.finite(max_time)) stop("max_time must be a finite numeric scalar.")
      }

      observe_fun <- self$bundle$observe
      do_obs <- isTRUE(return_observations) && !is.null(observe_fun)

      obs_list <- if (do_obs) vector("list", 0L) else NULL

      step_once <- function() {
        prop <- self$bundle$propose_event(patient, ctx = ctx)
        if (!is.list(prop) || is.null(prop$time_next) || is.null(prop$event_type)) {
          stop("bundle$propose_event must return a list with at least time_next and event_type.")
        }

        time_next <- as.numeric(prop$time_next)
        event_type <- as.character(prop$event_type)

        changes <- self$bundle$transition(patient, event_type = event_type, time_next = time_next, ctx = ctx)
        patient$update(time = time_next, event_type = event_type, changes = changes)

        if (do_obs) {
          obs <- observe_fun(patient, event_type = event_type, ctx = ctx)
          if (!is.list(obs)) obs <- list(value = obs)
          obs$j <- patient$last_j
          obs$time <- patient$last_time
          obs$event_type <- event_type
          obs_list[[length(obs_list) + 1L]] <<- obs
        }

        # Decide whether the simulation should stop *immediately* after this event.
        stop_now <- isTRUE(self$bundle$stop(patient, event_type = event_type, ctx = ctx))

        list(event_type = event_type, time = time_next, stop_now = stop_now)
      }

      last_event_type <- NA_character_
      for (i in seq_len(max_events)) {
        if (!is.null(max_time) && patient$last_time >= max_time) break

        res <- step_once()
        last_event_type <- res$event_type

        if (isTRUE(res$stop_now)) break
      }

      observations <- NULL
      if (do_obs && length(obs_list) > 0L) {
        observations <- do.call(rbind, lapply(obs_list, as.data.frame, stringsAsFactors = FALSE))
      }

      list(patient = patient, events = patient$events, observations = observations)
    }
  )
)

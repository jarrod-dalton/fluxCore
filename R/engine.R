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

.call_propose_events <- function(bundle, patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
  if (!is.null(bundle$propose_events) && is.function(bundle$propose_events)) {
    fml <- names(formals(bundle$propose_events))
    args <- list(patient = patient)
    if ("ctx" %in% fml) args$ctx <- ctx
    if ("process_ids" %in% fml) args$process_ids <- process_ids
    if ("current_proposals" %in% fml) args$current_proposals <- current_proposals
    out <- do.call(bundle$propose_events, args)
    if (is.null(out)) return(list())
    if (!is.list(out)) stop("bundle$propose_events must return a list of events keyed by process_id.")
    return(out)
  }

  if (!is.null(bundle$propose_event) && is.function(bundle$propose_event)) {
    ev <- bundle$propose_event(patient, ctx = ctx)
    if (is.null(ev)) return(list())
    if (!is.list(ev)) stop("bundle$propose_event must return a list(time_next, event_type, ...)")
    ev$process_id <- "default"
    return(list(default = ev))
  }

  stop("ModelBundle must provide propose_events() or propose_event().")
}

.pick_next_event <- function(proposals) {
  if (length(proposals) == 0L) stop("No proposals available.")
  times <- vapply(proposals, function(x) x$time_next, numeric(1))
  i <- which.min(times)
  pid <- names(proposals)[i]
  ev <- proposals[[pid]]
  ev$process_id <- pid
  ev
}

.call_transition <- function(bundle, patient, ev, ctx = NULL) {
  f <- bundle$transition
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide transition().")
  fml <- names(formals(f))
  if ("event" %in% fml) {
    args <- list(patient = patient, event = ev)
    if ("ctx" %in% fml) args$ctx <- ctx
    return(do.call(f, args))
  }
  args <- list(patient = patient, event_type = ev$event_type, time_next = ev$time_next)
  if ("ctx" %in% fml) args$ctx <- ctx
  return(do.call(f, args))
}

.call_stop <- function(bundle, patient, ev, ctx = NULL) {
  f <- bundle$stop
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide stop().")
  fml <- names(formals(f))
  if ("event" %in% fml) {
    args <- list(patient = patient, event = ev)
    if ("ctx" %in% fml) args$ctx <- ctx
    return(isTRUE(do.call(f, args)))
  }
  args <- list(patient = patient, event_type = ev$event_type)
  if ("ctx" %in% fml) args$ctx <- ctx
  return(isTRUE(do.call(f, args)))
}

.call_observe <- function(bundle, patient, ev, ctx = NULL) {
  f <- bundle$observe
  if (is.null(f) || !is.function(f)) return(NULL)
  fml <- names(formals(f))
  if ("event" %in% fml) {
    args <- list(patient = patient, event = ev)
    if ("ctx" %in% fml) args$ctx <- ctx
    return(do.call(f, args))
  }
  args <- list(patient = patient, event_type = ev$event_type)
  if ("ctx" %in% fml) args$ctx <- ctx
  return(do.call(f, args))
}

.call_refresh_rules <- function(bundle, patient, ev, changes, ctx = NULL) {
  f <- bundle$refresh_rules
  if (is.null(f) || !is.function(f)) return("ALL")
  fml <- names(formals(f))
  args <- list(patient = patient, last_event = ev, changes = changes)
  if ("ctx" %in% fml) args$ctx <- ctx
  out <- do.call(f, args)
  if (is.null(out)) return(character(0))
  out
}

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

    # Run a simulation for one patient
    # 
    # @param patient A `Patient` R6 object.
    # @param max_events Maximum number of additional events to simulate.
    # @param max_time Optional numeric; stop if patient time exceeds this value.
    # @param return_observations Logical; if TRUE, accumulate `bundle$observe()` outputs.
    # @param ctx Optional list passed through to bundle functions (e.g., draw_id, sim_id, params).
    # @return A list with elements:
    # - `patient`: the modified patient object
    # - `events`: the patient's event log
    # - `observations`: data.frame of observations (if requested and available)
    run = function(patient,
               max_events = 1000,
               max_time = NULL,
               return_observations = TRUE,
               ctx = NULL) {

  if (is.null(ctx)) ctx <- list()
  obs_accum <- NULL

  # initial proposals for all processes
  proposals <- .call_propose_events(self$bundle, patient, ctx = ctx)

  step_once <- function() {
    ev <- .pick_next_event(proposals)

    # apply transition patch (may be NULL)
    changes <- .call_transition(self$bundle, patient, ev, ctx = ctx)

    patient$update(time = ev$time_next, event_type = ev$event_type, changes = changes)

    if (return_observations) {
      o <- .call_observe(self$bundle, patient, ev, ctx = ctx)
      if (!is.null(o)) {
        obs_accum <<- if (is.null(obs_accum)) o else rbind(obs_accum, o)
      }
    }

    # stopping condition
    if (.call_stop(self$bundle, patient, ev, ctx = ctx)) {
      return(FALSE)
    }

    # time-based stopping condition
    if (!is.null(max_time) && patient$last_time >= max_time) {
      return(FALSE)
    }

    # refresh processes based on rules
    refresh_ids <- .call_refresh_rules(self$bundle, patient, ev, changes, ctx = ctx)
    if (identical(refresh_ids, "ALL")) {
      proposals <<- .call_propose_events(self$bundle, patient, ctx = ctx)
    } else if (length(refresh_ids) > 0) {
      # on-demand refresh: replace only those ids (bundle may or may not honor process_ids)
      new_props <- .call_propose_events(self$bundle, patient, ctx = ctx, process_ids = refresh_ids, current_proposals = proposals)
      for (pid in refresh_ids) {
        if (!is.null(new_props[[pid]])) {
          proposals[[pid]] <<- new_props[[pid]]
        } else {
          # if bundle returned nothing for pid, drop that process from cache
          proposals[[pid]] <<- NULL
        }
      }
    }

    TRUE
  }

  # iterate
  n <- 0L
  while (n < max_events) {
    n <- n + 1L
    cont <- step_once()
    if (!isTRUE(cont)) break
    if (length(proposals) == 0L) break
  }

  list(
    patient = patient,
    events = patient$events,
    observations = if (return_observations) obs_accum else NULL
  )
},
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

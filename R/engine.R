#' Engine
#'
#' Orchestrates simulation by repeatedly proposing the next event(s), applying a transition
#' patch, recording the event on a `Patient`, and stopping when `bundle$stop()` returns TRUE
#' (or a `max_time` / `max_events` limit is reached).
#'
#' The Engine supports both:
#'
#' - `bundle$propose_events(patient, ...)` returning multiple process-specific proposals, and
#' - legacy `bundle$propose_event(patient, ...)` returning a single proposal.
#'
#' When multiple processes are present, the Engine caches each process's next proposal and
#' refreshes proposals on-demand using `bundle$refresh_rules()` (default: refresh all).
#'
#' @section Fields:
#' - `provider`: a ModelProvider (e.g., [PackageProvider], [FileProvider])
#' - `model_spec`: named list describing what to load
#' - `bundle`: the loaded ModelBundle (named list of functions)
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

      if (!is.list(provider) && is.null(provider$load)) {
        stop("provider must be an object with a $load(model_spec, ...) method.")
      }
      if (!is.function(provider$load)) {
        stop("provider$load must be a function.")
      }

      self$bundle <- provider$load(model_spec = model_spec, ...)
      .validate_model_bundle(self$bundle)

      invisible(self)
    },

    # Run a simulation for one patient (see README for high-level flow)
    run = function(patient,
                   max_events = 1000,
                   max_time = NULL,
                   return_observations = TRUE,
                   ctx = NULL) {

      if (is.null(ctx)) ctx <- list()
      obs_accum <- NULL

      proposals <- .call_propose_events(self$bundle, patient, ctx = ctx)

      step_once <- function() {
        ev <- .pick_next_event(proposals)

        changes <- .call_transition(self$bundle, patient, ev, ctx = ctx)

        patient$update(time = ev$time_next, event_type = ev$event_type, changes = changes)

        if (isTRUE(return_observations)) {
          o <- .call_observe(self$bundle, patient, ev, ctx = ctx)
          if (!is.null(o)) {
            obs_accum <<- if (is.null(obs_accum)) o else rbind(obs_accum, o)
          }
        }

        if (.call_stop(self$bundle, patient, ev, ctx = ctx)) return(FALSE)
        if (!is.null(max_time) && patient$last_time >= max_time) return(FALSE)

        refresh_ids <- .call_refresh_rules(self$bundle, patient, ev, changes, ctx = ctx)

        if (identical(refresh_ids, "ALL")) {
          proposals <<- .call_propose_events(self$bundle, patient, ctx = ctx)
        } else if (length(refresh_ids) > 0) {
          new_props <- .call_propose_events(
            self$bundle, patient, ctx = ctx,
            process_ids = refresh_ids,
            current_proposals = proposals
          )
          for (pid in refresh_ids) {
            if (!is.null(new_props[[pid]])) {
              proposals[[pid]] <<- new_props[[pid]]
            } else {
              proposals[[pid]] <<- NULL
            }
          }
        }

        TRUE
      }

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
        observations = if (isTRUE(return_observations)) obs_accum else NULL
      )
    }
  )
)

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
  do.call(f, args)
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
  isTRUE(do.call(f, args))
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
  do.call(f, args)
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

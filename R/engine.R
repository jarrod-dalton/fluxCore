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

      # Time is a numeric axis shared across all processes. The Engine does not
      # enforce units, but models should declare the unit explicitly to avoid
      # silent mistakes when mixing rates/cadences (e.g., days vs years).
      if (is.null(ctx$time_unit) || !is.character(ctx$time_unit) || length(ctx$time_unit) != 1L || !nzchar(ctx$time_unit)) {
        warning(
          "ctx$time_unit is missing or invalid. Time is treated as unitless; set ctx$time_unit (e.g., 'days', 'months', 'years') for clarity.",
          call. = FALSE
        )
      }
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
  if (is.null(bundle$propose_events) || !is.function(bundle$propose_events)) {
    stop("ModelBundle must provide propose_events(patient, ctx, ...).")
  }

  fml <- names(formals(bundle$propose_events))
  args <- list(patient = patient)
  if ("ctx" %in% fml) args$ctx <- ctx
  if ("process_ids" %in% fml) args$process_ids <- process_ids
  if ("current_proposals" %in% fml) args$current_proposals <- current_proposals

  out <- do.call(bundle$propose_events, args)
  if (is.null(out)) return(list())
  if (!is.list(out)) stop("bundle$propose_events must return a list of events keyed by process_id.")
  out
}

.pick_next_event <- function(proposals) {
  if (length(proposals) == 0L) stop("No proposals available.")

  .validate_event <- function(x, pid) {
    if (is.null(x)) return(invisible(FALSE))
    if (!is.list(x)) stop(sprintf("Event proposal for process_id '%s' must be a list.", pid))
    if (is.null(x$time_next) || !is.numeric(x$time_next) || length(x$time_next) != 1L || !is.finite(x$time_next)) {
      stop(sprintf("Event proposal for process_id '%s' must include numeric scalar time_next.", pid))
    }
    if (is.null(x$event_type) || !is.character(x$event_type) || length(x$event_type) != 1L) {
      stop(sprintf("Event proposal for process_id '%s' must include character scalar event_type.", pid))
    }
    invisible(TRUE)
  }

  pids <- names(proposals)
  for (k in seq_along(proposals)) .validate_event(proposals[[k]], pids[[k]])

  times <- vapply(proposals, function(x) x$time_next, numeric(1))
  o <- order(times, pids) # deterministic tie-break: time, then process_id
  pid <- pids[[o[[1]]]]
  ev <- proposals[[pid]]
  ev$process_id <- pid
  ev
}


.call_transition <- function(bundle, patient, ev, ctx = NULL) {
  f <- bundle$transition
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide transition().")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("transition() must accept (patient, event, ...).")
  }

  args <- list(patient = patient, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  do.call(f, args)
}


.call_stop <- function(bundle, patient, ev, ctx = NULL) {
  f <- bundle$stop
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide stop().")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("stop() must accept (patient, event, ...).")
  }

  args <- list(patient = patient, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  isTRUE(do.call(f, args))
}


.call_observe <- function(bundle, patient, ev, ctx = NULL) {
  f <- bundle$observe
  if (is.null(f) || !is.function(f)) return(NULL)
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("observe() must accept (patient, event, ...).")
  }

  args <- list(patient = patient, event = ev)
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

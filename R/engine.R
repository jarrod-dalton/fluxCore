Engine <- R6::R6Class(
  classname = "Engine",
  public = list(
    provider = NULL,
    model_spec = NULL,
    bundle = NULL,
    time_spec = NULL,

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
      self$time_spec <- self$bundle$time_spec

      invisible(self)
    },

    # Run a simulation for one entity (see README for high-level flow)
    run = function(entity,
                   max_events = 1000,
                   max_time = NULL,
                   return_observations = TRUE,
                   ctx = NULL) {

      
if (is.null(ctx)) ctx <- list()
if (!is.list(ctx)) stop("ctx must be a list (or NULL).", call. = FALSE)
.assert_ctx_time_compatible(ctx = ctx, canonical_time_spec = self$time_spec, where = "Engine$run() ctx")
ctx$time <- .time_ctx_from_spec(self$time_spec)
ctx$time_spec <- self$time_spec

# Standardize model parameters in ctx$params.
# - Users may provide ctx$params to override defaults for a run.
# - Model bundles may provide bundle$params as a default.
if (is.null(ctx$params)) {
  if (!is.null(self$bundle$params)) {
    if (!is.list(self$bundle$params)) stop("bundle$params must be a list if provided.", call. = FALSE)
    ctx$params <- self$bundle$params
  } else {
    ctx$params <- list()
  }
} else {
  if (!is.list(ctx$params)) stop("ctx$params must be a list if provided.", call. = FALSE)
}

# One-time initialization hook (optional).
# Models can use this to register derived variables and perform setup.
.call_init_entity(self$bundle, entity, ctx = ctx)

      obs_accum <- NULL

      proposals <- .call_propose_events(self$bundle, entity, ctx = ctx)

      step_once <- function() {
        ev <- .pick_next_event(proposals)

        changes <- .call_transition(self$bundle, entity, ev, ctx = ctx)

        entity$update(time = ev$time_next, event_type = ev$event_type, changes = changes)

        if (isTRUE(return_observations)) {
          o <- .call_observe(self$bundle, entity, ev, ctx = ctx)
          if (!is.null(o)) {
            obs_accum <<- if (is.null(obs_accum)) o else rbind(obs_accum, o)
          }
        }

        if (.call_stop(self$bundle, entity, ev, ctx = ctx)) return(FALSE)
        if (!is.null(max_time) && entity$last_time >= max_time) return(FALSE)

        refresh_ids <- .call_refresh_rules(self$bundle, entity, ev, changes, ctx = ctx)

        if (identical(refresh_ids, "ALL")) {
          proposals <<- .call_propose_events(self$bundle, entity, ctx = ctx)
        } else if (length(refresh_ids) > 0) {
          new_props <- .call_propose_events(
            self$bundle, entity, ctx = ctx,
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
        entity = entity,
        events = entity$events,
        observations = if (isTRUE(return_observations)) obs_accum else NULL
      )
    }
  )
)

.call_init_entity <- function(bundle, entity, ctx = NULL) {
  f <- bundle$init_entity
  if (is.null(f)) return(invisible(NULL))
  if (!is.function(f)) stop("init_entity must be a function if provided.", call. = FALSE)

  fml <- names(formals(f))
  args <- list(entity = entity)
  if ("ctx" %in% fml) args$ctx <- ctx
  invisible(do.call(f, args))
}

.call_propose_events <- function(bundle, entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
  if (is.null(bundle$propose_events) || !is.function(bundle$propose_events)) {
    stop("ModelBundle must provide propose_events(entity, ctx, ...).")
  }

  fml <- names(formals(bundle$propose_events))
  args <- list(entity = entity)
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


.call_transition <- function(bundle, entity, ev, ctx = NULL) {
  f <- bundle$transition
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide transition().")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("transition() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  do.call(f, args)
}


.call_stop <- function(bundle, entity, ev, ctx = NULL) {
  f <- bundle$stop
  if (is.null(f) || !is.function(f)) stop("ModelBundle must provide stop().")
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("stop() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  isTRUE(do.call(f, args))
}


.call_observe <- function(bundle, entity, ev, ctx = NULL) {
  f <- bundle$observe
  if (is.null(f) || !is.function(f)) return(NULL)
  fml <- names(formals(f))

  if (!("event" %in% fml)) {
    stop("observe() must accept (entity, event, ...).")
  }

  args <- list(entity = entity, event = ev)
  if ("ctx" %in% fml) args$ctx <- ctx
  do.call(f, args)
}

.call_refresh_rules <- function(bundle, entity, ev, changes, ctx = NULL) {
  f <- bundle$refresh_rules
  if (is.null(f) || !is.function(f)) return("ALL")
  fml <- names(formals(f))
  args <- list(entity = entity, last_event = ev, changes = changes)
  if ("ctx" %in% fml) args$ctx <- ctx
  out <- do.call(f, args)
  if (is.null(out)) return(character(0))
  out
}

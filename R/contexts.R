# contexts.R ---------------------------------------------------------------
#
# Typed context constructors and validators introduced in v2.0.0.
#
# These replace the catch-all `ctx` list that was the v1.x convention.
# Engine entry points built via load_model() will receive and propagate
# typed context objects. Direct Engine$new() construction retains v1.x
# behaviour (see engine.R for v2_mode flag).
#
# Context objects are plain R lists with a class attribute, making them
# easy to inspect, print, and serialise to JSON.
# --------------------------------------------------------------------------

# SimContext ----------------------------------------------------------------

#' Construct a SimContext
#'
#' Captures invariant simulation-level metadata for one Engine run. Created
#' once by `load_model()` / `Engine$run()` in v2 mode and passed to every
#' bundle callback that accepts a `sim_ctx` argument.
#'
#' @param run_id Character scalar; unique identifier for this simulation run.
#' @param time_spec A `time_spec` object (from [time_spec()]). Resolved from
#'   the model schema.
#' @param model_id Optional character scalar identifying the model.
#' @param scenario_id Optional character scalar labelling an experimental
#'   condition.
#' @param horizon Optional numeric scalar declaring the simulation horizon.
#'
#' @return A list of class `"SimContext"`.
#'
#' @export
SimContext <- function(run_id,
                       time_spec,
                       model_id    = NULL,
                       scenario_id = NULL,
                       horizon     = NULL) {
  if (missing(run_id) || !is.character(run_id) || length(run_id) != 1L || is.na(run_id) || !nzchar(run_id)) {
    stop("SimContext: `run_id` must be a non-empty character scalar.", call. = FALSE)
  }
  if (missing(time_spec) || !inherits(time_spec, "time_spec")) {
    stop("SimContext: `time_spec` must be a `time_spec` object (see time_spec()).", call. = FALSE)
  }
  if (!is.null(model_id)) {
    if (!is.character(model_id) || length(model_id) != 1L) {
      stop("SimContext: `model_id` must be a character scalar or NULL.", call. = FALSE)
    }
  }
  if (!is.null(scenario_id)) {
    if (!is.character(scenario_id) || length(scenario_id) != 1L) {
      stop("SimContext: `scenario_id` must be a character scalar or NULL.", call. = FALSE)
    }
  }
  if (!is.null(horizon)) {
    horizon <- suppressWarnings(as.numeric(horizon))
    if (length(horizon) != 1L || !is.finite(horizon) || horizon <= 0) {
      stop("SimContext: `horizon` must be a positive finite numeric scalar or NULL.", call. = FALSE)
    }
  }

  structure(
    list(
      run_id      = run_id,
      time_spec   = time_spec,
      model_id    = model_id,
      scenario_id = scenario_id,
      horizon     = horizon
    ),
    class = "SimContext"
  )
}

#' @export
print.SimContext <- function(x, ...) {
  cat("<SimContext>\n")
  cat("  run_id     :", x$run_id, "\n")
  cat("  model_id   :", if (is.null(x$model_id)) "(none)" else x$model_id, "\n")
  cat("  scenario_id:", if (is.null(x$scenario_id)) "(none)" else x$scenario_id, "\n")
  cat("  horizon    :", if (is.null(x$horizon)) "(none)" else x$horizon, "\n")
  invisible(x)
}


# ParamContext --------------------------------------------------------------

#' Construct a ParamContext
#'
#' Encapsulates one parameter realization for a simulation run. Resolved once
#' per draw by a `ParamSource` (or supplied directly) and injected into every
#' bundle callback that accepts a `param_ctx` argument.
#'
#' @param draw_id Positive integer-valued numeric scalar identifying this
#'   parameter draw. Whole-valued doubles such as `5.0` are accepted and stored
#'   as integers; fractional, non-finite, non-positive, and out-of-range values
#'   are rejected rather than truncated.
#' @param params Named list of concrete parameter values.
#' @param provenance Optional character scalar labelling the source of this
#'   draw (e.g., `"posterior_draw_42"`).
#'
#' @return A list of class `"ParamContext"`.
#'
#' @export
ParamContext <- function(draw_id, params, provenance = NULL) {
  if (missing(draw_id) ||
      !is.numeric(draw_id) ||
      length(draw_id) != 1L ||
      is.na(draw_id) ||
      !is.finite(draw_id) ||
      draw_id <= 0 ||
      draw_id > .Machine$integer.max ||
      draw_id != floor(draw_id)) {
    stop(
      "ParamContext: `draw_id` must be a positive, losslessly integer-valued numeric scalar.",
      call. = FALSE
    )
  }
  draw_id <- as.integer(draw_id)
  if (missing(params) || !is.list(params)) {
    stop("ParamContext: `params` must be a named list.", call. = FALSE)
  }
  if (!is.null(provenance)) {
    if (!is.character(provenance) || length(provenance) != 1L) {
      stop("ParamContext: `provenance` must be a character scalar or NULL.", call. = FALSE)
    }
  }

  structure(
    list(draw_id = draw_id, params = params, provenance = provenance),
    class = "ParamContext"
  )
}

#' @export
print.ParamContext <- function(x, ...) {
  cat("<ParamContext>\n")
  cat("  draw_id   :", x$draw_id, "\n")
  cat("  provenance:", if (is.null(x$provenance)) "(none)" else x$provenance, "\n")
  cat("  params    :", length(x$params), "field(s)\n")
  invisible(x)
}


# RuntimeContext ------------------------------------------------------------

#' Construct a RuntimeContext
#'
#' Captures reproducibility and backend settings. A context stored on an Engine
#' configures direct `Engine$run()` calls. When supplied explicitly to
#' [run_cohort()], it configures the cohort and takes precedence over scalar
#' seed/backend/worker controls; its `replicate_id` must be `NULL` because
#' cohort replication is identified by `sim_id`.
#'
#' The public reproducibility contract is:
#'   `seed + draw_id + replicate_id + entity_id` = deterministic output.
#' Internal stream allocation (`stream_id`) is set by the run harness and
#' must not be set by users directly.
#'
#' @param seed Optional integer scalar; top-level seed. NULL = unseeded.
#' @param replicate_id Optional integer scalar; stochastic replicate index for
#'   a direct Engine run. Cohorts use `sim_id` and reject a non-`NULL` value on
#'   an explicitly supplied RuntimeContext.
#' @param backend Character scalar; one of `"none"` (default), `"cluster"`,
#'   `"mclapply"`, `"future"`.
#' @param n_workers Optional integer; number of parallel workers.
#'
#' @return A list of class `"RuntimeContext"`.
#'
#' @export
RuntimeContext <- function(seed         = NULL,
                           replicate_id = NULL,
                           backend      = "none",
                           n_workers    = NULL) {
  if (!is.null(seed)) {
    seed <- suppressWarnings(as.integer(seed))
    if (length(seed) != 1L || is.na(seed)) {
      stop("RuntimeContext: `seed` must be a single integer-coercible value or NULL.", call. = FALSE)
    }
  }
  if (!is.null(replicate_id)) {
    replicate_id <- suppressWarnings(as.integer(replicate_id))
    if (length(replicate_id) != 1L || is.na(replicate_id)) {
      stop("RuntimeContext: `replicate_id` must be a single integer-coercible value or NULL.", call. = FALSE)
    }
  }
  valid_backends <- c("none", "cluster", "mclapply", "future")
  if (!is.character(backend) || length(backend) != 1L || !backend %in% valid_backends) {
    stop(sprintf("RuntimeContext: `backend` must be one of: %s.", paste(valid_backends, collapse = ", ")), call. = FALSE)
  }
  if (!is.null(n_workers)) {
    n_workers <- suppressWarnings(as.integer(n_workers))
    if (length(n_workers) != 1L || is.na(n_workers) || n_workers < 1L) {
      stop("RuntimeContext: `n_workers` must be a positive integer or NULL.", call. = FALSE)
    }
  }

  structure(
    list(
      seed         = seed,
      replicate_id = replicate_id,
      backend      = backend,
      n_workers    = n_workers,
      stream_id    = NULL  # internal; set by run harness only
    ),
    class = "RuntimeContext"
  )
}

#' @export
print.RuntimeContext <- function(x, ...) {
  cat("<RuntimeContext>\n")
  cat("  seed        :", if (is.null(x$seed)) "(unseeded)" else x$seed, "\n")
  cat("  replicate_id:", if (is.null(x$replicate_id)) "(none)" else x$replicate_id, "\n")
  cat("  backend     :", x$backend, "\n")
  cat("  n_workers   :", if (is.null(x$n_workers)) "(auto)" else x$n_workers, "\n")
  invisible(x)
}


# EnvironmentContext --------------------------------------------------------

#' Construct an EnvironmentContext
#'
#' Optional. Provides named external signals and world-step/reset hooks for
#' ABM or RL scenarios. Designed to accommodate multi-entity ABM (an add-on
#' sub-repo) without requiring Engine architectural changes.
#'
#' @param signals Optional named list of external signal values visible to
#'   the policy at each decision point.
#' @param step_fn Optional function; advances the world by one step
#'   (ABM/RL use cases).
#' @param reset_fn Optional function; resets the world state (RL use cases).
#' @param info Optional named list of auxiliary metadata from the environment.
#'
#' @return A list of class `"EnvironmentContext"`.
#'
#' @export
EnvironmentContext <- function(signals  = NULL,
                               step_fn  = NULL,
                               reset_fn = NULL,
                               info     = NULL) {
  if (!is.null(signals) && (!is.list(signals) || is.null(names(signals)))) {
    stop("EnvironmentContext: `signals` must be a named list or NULL.", call. = FALSE)
  }
  if (!is.null(step_fn) && !is.function(step_fn)) {
    stop("EnvironmentContext: `step_fn` must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(reset_fn) && !is.function(reset_fn)) {
    stop("EnvironmentContext: `reset_fn` must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(info) && !is.list(info)) {
    stop("EnvironmentContext: `info` must be a list or NULL.", call. = FALSE)
  }

  structure(
    list(signals = signals, step_fn = step_fn, reset_fn = reset_fn, info = info),
    class = "EnvironmentContext"
  )
}

#' @export
print.EnvironmentContext <- function(x, ...) {
  cat("<EnvironmentContext>\n")
  cat("  signals :", if (is.null(x$signals)) "(none)" else paste(length(x$signals), "signal(s)"), "\n")
  cat("  step_fn :", if (is.null(x$step_fn)) "(none)" else "(provided)", "\n")
  cat("  reset_fn:", if (is.null(x$reset_fn)) "(none)" else "(provided)", "\n")
  invisible(x)
}

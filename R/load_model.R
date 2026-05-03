# load_model.R -------------------------------------------------------------
#
# load_model() is the v2.0.0 recommended assembly and validation function.
# It validates schema, bundle, and optional components against each other,
# applies defaults, sets v2_mode on the Engine, and returns a configured
# Engine ready to run.
#
# Direct Engine$new() construction is still supported for advanced use and
# backward compatibility with v1.x tests, but does NOT enforce the v2
# contracts.
# --------------------------------------------------------------------------

#' Assemble and validate a simulation model (v2.0.0 API)
#'
#' `load_model()` is the recommended entry point for the v2.0.0 architecture.
#' It validates all supplied components against the schema and against each
#' other, applies defaults, and returns a configured [Engine] in v2 mode.
#'
#' Engines returned by `load_model()` enforce the following at runtime:
#' - **Fail fast on `ctx`-style usage**: passing a `ctx=` argument to
#'   `Engine$run()` raises an immediate error. Use typed context objects
#'   (`SimContext`, `ParamContext`, `RuntimeContext`) instead.
#' - All bundle callbacks must accept `sim_ctx` and `param_ctx` in their
#'   formals (warnings emitted for v1.x-style `ctx` formals).
#'
#' @section User experience tiers:
#' | Level | Entry point | What you supply |
#' |-------|-------------|-----------------|
#' | 1     | `load_model(schema, bundle)` | In-memory schema + bundle |
#' | 2     | `load_model(schema, bundle, ...)` + JSON schema export | JSON spec + language code |
#' | 3     | All optional components supplied | Full stack |
#'
#' @section Trajectory output contract:
#' When `trajectory` is configured, the Engine returned by `load_model()` emits
#' `trajectory_records` in run outputs.
#'
#' - `Engine$run(...)` includes `trajectory_records` when trajectory logging is enabled.
#' - `run_cohort(...)` run entries include per-run `trajectory_records` when enabled.
#' - `trajectory_records` is a list of plain named lists (JSON-serializable).
#' - `trajectory$detail` controls state capture:
#'   - `none`: `state_before` and `state_after` are `NULL`.
#'   - `summary`: both are outputs of `summary_fn` (default [state_summary_default()]).
#'   - `full`: both are full snapshots of `entity$current`.
#'
#' @param schema A validated schema list (from [set_schema()] or equivalent),
#'   **or** a schema-like named list. Must include at minimum a `$variables`
#'   field and a `$time_spec` of class `"time_spec"`. See [set_schema()].
#' @param bundle A ModelBundle list with at minimum `propose_events`,
#'   `transition`, and `stop` callbacks.
#' @param policy Optional. A function or list with a `propose_action` method
#'   called at declared decision points. Stage 2B.
#' @param environment Optional. An [EnvironmentContext] for ABM/RL scenarios.
#' @param trajectory Optional. A TrajectoryLogger configuration list; enables
#'   [TrajectoryRecord] emission. Requires `schema$decision_points` to be
#'   non-empty.
#' @param runtime Optional. A [RuntimeContext] specifying reproducibility and
#'   backend settings. Defaults are applied when `NULL`.
#' @param param_source Optional. A ParamSource that resolves
#'   [ParamContext]s once per run.
#'
#' @return An [Engine] object with `v2_mode = TRUE`.
#'
#' @seealso [SimContext()], [ParamContext()], [RuntimeContext()],
#'   [EnvironmentContext()], [DecisionPoint()], [TrajectoryRecord()]
#'
#' @export
load_model <- function(schema,
                       bundle,
                       policy       = NULL,
                       environment  = NULL,
                       trajectory   = NULL,
                       runtime      = NULL,
                       param_source = NULL) {

  # -- schema ----------------------------------------------------------------
  if (missing(schema) || is.null(schema)) {
    stop("load_model(): `schema` is required.", call. = FALSE)
  }
  if (!is.list(schema)) {
    stop("load_model(): `schema` must be a list.", call. = FALSE)
  }

  # time_spec: required for SimContext construction
  ts <- schema$time_spec
  if (is.null(ts) && !is.null(schema$variables)) {
    # Allow schema created via set_schema() which stores time_spec separately
    # on bundle$time_spec. Fall through to bundle check below.
    ts <- NULL
  }
  if (!is.null(ts) && !inherits(ts, "time_spec")) {
    stop("load_model(): `schema$time_spec` must be a `time_spec` object.", call. = FALSE)
  }

  # -- bundle ----------------------------------------------------------------
  if (missing(bundle) || is.null(bundle)) {
    stop("load_model(): `bundle` is required.", call. = FALSE)
  }
  .validate_model_bundle(bundle)

  # Resolve time_spec: prefer schema$time_spec, fall back to bundle$time_spec
  if (is.null(ts)) {
    ts <- bundle$time_spec
  }
  if (is.null(ts) || !inherits(ts, "time_spec")) {
    stop(
      "load_model(): a `time_spec` object must be provided in `schema$time_spec` or `bundle$time_spec`.",
      call. = FALSE
    )
  }

  # Warn on v1.x-style `ctx` formals in bundle callbacks (not an error yet;
  # Stage 4 will convert downstream packages)
  .warn_ctx_formals(bundle)

  # -- trajectory + decision_points check ------------------------------------
  if (!is.null(trajectory)) {
    dps <- schema$decision_points
    if (is.null(dps) || length(dps) == 0L) {
      stop(
        "load_model(): `trajectory` logger requires at least one DecisionPoint in `schema$decision_points`.",
        call. = FALSE
      )
    }
    trajectory <- .normalize_trajectory_logger(trajectory)
  }

  # -- environment -----------------------------------------------------------
  if (!is.null(environment) && !inherits(environment, "EnvironmentContext")) {
    stop("load_model(): `environment` must be an EnvironmentContext or NULL.", call. = FALSE)
  }

  # -- runtime ---------------------------------------------------------------
  if (is.null(runtime)) {
    runtime <- RuntimeContext()
  } else if (!inherits(runtime, "RuntimeContext")) {
    stop("load_model(): `runtime` must be a RuntimeContext or NULL.", call. = FALSE)
  }

  # -- param_source ----------------------------------------------------------
  # Validate shape against schema$param_schema if both are provided
  if (!is.null(param_source) && !is.null(schema$param_schema)) {
    if (!is.list(param_source) && !is.function(param_source$sample)) {
      stop("load_model(): `param_source` must expose a `sample(n)` function.", call. = FALSE)
    }
  }

  # -- assemble Engine in v2 mode --------------------------------------------
  engine <- Engine$new(bundle = bundle)
  engine$.v2_mode     <- TRUE
  engine$.schema      <- schema
  engine$.policy      <- policy
  engine$.environment <- environment
  engine$.trajectory  <- trajectory
  engine$.runtime     <- runtime
  engine$.param_source <- param_source
  engine$.time_spec   <- ts

  invisible(engine)
}


# Internal helpers ----------------------------------------------------------

# Warn when bundle callbacks still use the v1.x `ctx` formal name.
# This is advisory in Stage 2; hard errors will be introduced in Stage 4
# when downstream packages are migrated.
.warn_ctx_formals <- function(bundle) {
  ctx_callbacks <- c("propose_events", "transition", "stop", "observe",
                     "refresh_rules", "init_entity")
  for (cb in ctx_callbacks) {
    f <- bundle[[cb]]
    if (!is.function(f)) next
    fml <- names(formals(f))
    if ("ctx" %in% fml) {
      warning(
        sprintf(
          "load_model(): bundle$%s() uses a `ctx` formal argument (v1.x convention). ",
          cb
        ),
        "Update to `sim_ctx` and `param_ctx` before Stage 4.",
        call. = FALSE
      )
    }
  }
}

# Normalize trajectory logger configuration.
# Supported fields:
#   detail: "none" | "summary" | "full" (default: "none")
#   summary_fn: function(entity, ...) -> named list (used when detail = "summary")
.normalize_trajectory_logger <- function(trajectory) {
  if (!is.list(trajectory)) {
    stop("load_model(): `trajectory` must be a list or NULL.", call. = FALSE)
  }

  detail <- trajectory$detail
  if (is.null(detail)) detail <- "none"
  if (!is.character(detail) || length(detail) != 1L || !nzchar(detail)) {
    stop("load_model(): `trajectory$detail` must be one of 'none', 'summary', or 'full'.", call. = FALSE)
  }
  if (!(detail %in% c("none", "summary", "full"))) {
    stop("load_model(): `trajectory$detail` must be one of 'none', 'summary', or 'full'.", call. = FALSE)
  }

  summary_fn <- trajectory$summary_fn
  if (is.null(summary_fn)) summary_fn <- state_summary_default
  if (!is.function(summary_fn)) {
    stop("load_model(): `trajectory$summary_fn` must be a function or NULL.", call. = FALSE)
  }

  list(
    detail = detail,
    summary_fn = summary_fn
  )
}

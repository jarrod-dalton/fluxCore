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

#' Assemble and validate a simulation model (v2 API)
#'
#' `load_model()` is the recommended entry point for the v2 architecture.
#' It validates all supplied components against the schema and against each
#' other, applies defaults, and returns a configured [Engine] in v2 mode.
#'
#' Engines returned by `load_model()` enforce the following at runtime:
#' - **Fail fast on `ctx`-style usage**: passing a `ctx=` argument to
#'   `Engine$run()` raises an immediate error. Use typed context objects
#'   (`SimContext`, `ParamContext`, `RuntimeContext`) instead.
#' - Bundle callbacks that need typed context declare supported `sim_ctx` and
#'   `param_ctx` formals. The removed v1.x-style `ctx` formal is rejected.
#'
#' @section Model time contract:
#' A full schema and its ModelBundle must declare semantically equal
#' `time_spec` objects. Equality covers unit, origin instant, origin class, and
#' zone; object identity is not required. [Engine]`$time_spec` is the sole
#' assembled runtime clock and is propagated through `SimContext`.
#'
#' For 2.1 compatibility, a variables-only schema (a named list of variable
#' definitions without the full `$variables` wrapper) is accepted with a
#' migration warning and uses `bundle$time_spec`. A full schema, identified by
#' the presence of a `$variables` field, must also contain `$time_spec`.
#'
#' @section Decision declaration contract:
#' Leaf [DecisionPoint()] objects remain in `schema$decision_points`. Shared
#' trigger declarations live separately in `schema$decision_groups` and
#' reference leaf ids rather than embedding leaf definitions. `load_model()`
#' defensively repeats the global id, membership, no-nesting, and group-only
#' trigger checks performed by [set_schema()] so manually assembled schemas do
#' not bypass the declaration contract. When at least one group is declared,
#' `policy` must be a list exposing `propose_plan()`; grouped dispatch never
#' falls back implicitly to `propose_action()`.
#'
#' For each fired group with at least one eligible member, Core makes exactly
#' one call of the following shape:
#'
#' ```r
#' policy$propose_plan(
#'   grouped_decision_point,
#'   eligible_decision_points,
#'   entity,
#'   sim_ctx = NULL,
#'   param_ctx = NULL
#' )
#' ```
#'
#' The first three named inputs are required. Core supplies `sim_ctx` and
#' `param_ctx` only when the callback declares those formals. The eligible
#' decision points are canonical schema objects in group member order, evaluated
#' after the triggering transition; when none are eligible, Core skips the
#' callback. The result must be a complete [DecisionPlan()] rather than `NULL`.
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
#' @param schema A validated full schema list (from [set_schema()] or
#'   equivalent) containing at minimum `$variables` and a `$time_spec` of class
#'   `"time_spec"`. For 2.1 compatibility, a variables-only named list is also
#'   accepted with a migration warning. A full schema may contain canonical leaf
#'   declarations in `$decision_points` and grouped references in
#'   `$decision_groups`. See [set_schema()].
#' @param bundle A ModelBundle list with at minimum `propose_events`,
#'   `transition`, and `stop` callbacks, plus a `$time_spec` semantically equal
#'   to the full schema declaration.
#' @param policy Optional for ordinary decisions. A function or list with a
#'   `propose_action` method called at ordinary decision points. A schema with
#'   non-empty `decision_groups` requires a list with an exact `propose_plan`
#'   callback as documented above; there is no implicit ordinary-policy
#'   fallback.
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
#'   [EnvironmentContext()], [DecisionPoint()], [GroupedDecisionPoint()],
#'   [DecisionPlan()], [TrajectoryRecord()]
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

  # A `$variables` field is the explicit discriminator for the full-schema
  # shape. A raw named list of variable definitions remains a compatibility
  # input in 2.1, but it does not carry a separate model-clock declaration.
  full_schema <- "variables" %in% names(schema)
  schema_time_spec <- NULL
  if (full_schema) {
    if (!("time_spec" %in% names(schema)) || is.null(schema$time_spec)) {
      stop(
        "load_model(): a full schema (one containing `$variables`) must define `schema$time_spec`.",
        call. = FALSE
      )
    }
    schema_time_spec <- schema$time_spec
    if (!inherits(schema_time_spec, "time_spec")) {
      stop("load_model(): `schema$time_spec` must be a `time_spec` object.", call. = FALSE)
    }
  }

  # -- bundle ----------------------------------------------------------------
  if (missing(bundle) || is.null(bundle)) {
    stop("load_model(): `bundle` is required.", call. = FALSE)
  }
  .validate_model_bundle(bundle)

  # Full loaded models have one clock represented in both declarations. There
  # is no precedence or cross-unit conversion: disagreement is an assembly
  # error. The bundle carries the accepted clock into Engine runtime.
  if (full_schema && !.time_spec_equal(schema_time_spec, bundle$time_spec)) {
    stop(
      "load_model(): `schema$time_spec` and `bundle$time_spec` must be semantically equal ",
      "(unit, origin instant, origin class, and zone). ",
      "Got schema$time_spec ", .describe_time_spec(schema_time_spec),
      " and bundle$time_spec ", .describe_time_spec(bundle$time_spec), ".",
      call. = FALSE
    )
  }
  if (!full_schema) {
    warning(
      "load_model(): a variables-only schema was supplied without the full `$variables`/`$time_spec` contract; ",
      "using `bundle$time_spec` for 2.1 compatibility. Build a full schema with `set_schema(..., time_spec = ...)`.",
      call. = FALSE
    )
  }

  # v2.0.0: hard error on bundle callbacks that still declare `ctx`
  .reject_ctx_formals(bundle)

  # -- decision declaration graph -------------------------------------------
  # Repeat the essential constructor and cross-reference checks defensively:
  # callers may supply a manually assembled or modified full schema rather than
  # the value returned directly by set_schema(). No-group schemas follow the
  # same leaf validation and retain their existing declaration order.
  if (full_schema) {
    .validate_decision_schema_contract(
      decision_points = schema$decision_points,
      decision_groups = schema$decision_groups,
      caller = "load_model"
    )
    if (!is.null(schema$decision_groups) && length(schema$decision_groups) > 0L &&
        (!is.list(policy) || !is.function(policy$propose_plan))) {
      stop(
        "load_model(): non-empty `schema$decision_groups` requires `policy` to be a list with a `propose_plan()` function.",
        call. = FALSE
      )
    }
  }

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

  invisible(engine)
}


# Internal helpers ----------------------------------------------------------

# Hard error when bundle callbacks use the removed v1.x `ctx` formal name.
.reject_ctx_formals <- function(bundle) {
  ctx_callbacks <- c("propose_events", "transition", "stop", "observe",
                     "refresh_rules", "init_entity")
  for (cb in ctx_callbacks) {
    f <- bundle[[cb]]
    if (!is.function(f)) next
    fml <- names(formals(f))
    if ("ctx" %in% fml) {
      stop(
        sprintf(
          "load_model(): bundle$%s() declares `ctx` as a formal parameter. ", cb
        ),
        "`ctx` is removed in fluxCore v2.0.0. ",
        "Declare one model clock in `schema$time_spec` and `bundle$time_spec`; ",
        "callbacks that accept `sim_ctx` can read it from `sim_ctx$time_spec`.",
        call. = FALSE
      )
    }
  }
}

# Concise, deterministic description for time-contract mismatch diagnostics.
.describe_time_spec <- function(x) {
  origin_instant <- format(
    as.numeric(x$origin_posix),
    scientific = FALSE,
    trim = TRUE,
    digits = 15
  )
  sprintf(
    "{unit='%s', origin_instant=%s, origin_class='%s', zone='%s'}",
    x$unit,
    origin_instant,
    x$origin_class,
    x$zone
  )
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

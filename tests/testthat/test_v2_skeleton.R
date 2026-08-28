## Tests for Stage 2 skeleton constructs:
##   SimContext, ParamContext, RuntimeContext, EnvironmentContext
##   DecisionPoint, dp_fires(), ActionEvent, TrajectoryRecord
##   state_summary_default(), load_model()

# ---- SimContext ------------------------------------------------------------

test_that("SimContext: constructs valid object", {
  ts <- time_spec(unit = "years")
  sc <- SimContext(run_id = "run-001", time_spec = ts)
  expect_s3_class(sc, "SimContext")
  expect_equal(sc$run_id, "run-001")
  expect_null(sc$model_id)
  expect_null(sc$scenario_id)
  expect_null(sc$horizon)
})

test_that("SimContext: accepts optional fields", {
  ts <- time_spec(unit = "days")
  sc <- SimContext("r1", ts, model_id = "m1", scenario_id = "s1", horizon = 10)
  expect_equal(sc$model_id, "m1")
  expect_equal(sc$scenario_id, "s1")
  expect_equal(sc$horizon, 10)
})

test_that("SimContext: errors on bad run_id", {
  ts <- time_spec(unit = "years")
  expect_error(SimContext(run_id = "",    time_spec = ts), "run_id")
  expect_error(SimContext(run_id = NA_character_, time_spec = ts), "run_id")
  expect_error(SimContext(run_id = 123,   time_spec = ts), "run_id")
})

test_that("SimContext: errors if time_spec is wrong class", {
  expect_error(SimContext("r1", time_spec = list(unit = "years")), "time_spec")
})

test_that("SimContext: errors on non-positive horizon", {
  ts <- time_spec(unit = "years")
  expect_error(SimContext("r1", ts, horizon = -1), "horizon")
  expect_error(SimContext("r1", ts, horizon = 0),  "horizon")
})

test_that("SimContext: print method runs without error", {
  ts <- time_spec(unit = "years")
  sc <- SimContext("r1", ts)
  expect_output(print(sc), "SimContext")
})

# ---- ParamContext ----------------------------------------------------------

test_that("ParamContext: constructs valid object", {
  pc <- ParamContext(draw_id = 1L, params = list(rate = 0.1))
  expect_s3_class(pc, "ParamContext")
  expect_equal(pc$draw_id, 1L)
  expect_equal(pc$params$rate, 0.1)
  expect_null(pc$provenance)
})

test_that("ParamContext: coerces numeric draw_id to integer", {
  pc <- ParamContext(draw_id = 5.0, params = list())
  expect_identical(pc$draw_id, 5L)
})

test_that("ParamContext: errors on invalid draw_id", {
  expect_error(ParamContext(draw_id = "x", params = list()), "draw_id")
  expect_error(ParamContext(draw_id = 1.2, params = list()), "draw_id")
  expect_error(ParamContext(draw_id = 0L, params = list()), "draw_id")
})

test_that("ParamContext: errors when params is not a list", {
  expect_error(ParamContext(draw_id = 1L, params = "bad"), "params")
})

test_that("ParamContext: print method runs without error", {
  pc <- ParamContext(1L, list(a = 1))
  expect_output(print(pc), "ParamContext")
})

# ---- RuntimeContext --------------------------------------------------------

test_that("RuntimeContext: constructs with defaults", {
  rc <- RuntimeContext()
  expect_s3_class(rc, "RuntimeContext")
  expect_null(rc$seed)
  expect_null(rc$replicate_id)
  expect_equal(rc$backend, "none")
  expect_null(rc$n_workers)
  expect_null(rc$stream_id)
})

test_that("RuntimeContext: accepts valid seed and replicate_id", {
  rc <- RuntimeContext(seed = 42L, replicate_id = 3L)
  expect_equal(rc$seed, 42L)
  expect_equal(rc$replicate_id, 3L)
})

test_that("RuntimeContext: errors on invalid backend", {
  expect_error(RuntimeContext(backend = "spark"), "backend")
})

test_that("RuntimeContext: errors on non-positive n_workers", {
  expect_error(RuntimeContext(n_workers = 0L), "n_workers")
})

test_that("RuntimeContext: stream_id is always NULL on construction (internal use only)", {
  rc <- RuntimeContext(seed = 1L)
  expect_null(rc$stream_id)
})

test_that("RuntimeContext: print method runs without error", {
  rc <- RuntimeContext(seed = 7L, backend = "cluster", n_workers = 4L)
  expect_output(print(rc), "RuntimeContext")
})

# ---- EnvironmentContext ---------------------------------------------------

test_that("EnvironmentContext: constructs empty", {
  ec <- EnvironmentContext()
  expect_s3_class(ec, "EnvironmentContext")
  expect_null(ec$signals)
  expect_null(ec$step_fn)
})

test_that("EnvironmentContext: accepts signals and functions", {
  ec <- EnvironmentContext(
    signals  = list(temp = 20),
    step_fn  = function() NULL,
    reset_fn = function() NULL
  )
  expect_equal(ec$signals$temp, 20)
  expect_true(is.function(ec$step_fn))
})

test_that("EnvironmentContext: errors on unnamed signals list", {
  expect_error(EnvironmentContext(signals = list(1, 2)), "signals")
})

test_that("EnvironmentContext: errors on non-function step_fn", {
  expect_error(EnvironmentContext(step_fn = "bad"), "step_fn")
})

test_that("EnvironmentContext: print method runs without error", {
  ec <- EnvironmentContext(signals = list(x = 1))
  expect_output(print(ec), "EnvironmentContext")
})

# ---- DecisionPoint --------------------------------------------------------

test_that("DecisionPoint: constructs with event type trigger", {
  dp <- DecisionPoint(id = "post_dropoff", trigger = "dropoff")
  expect_s3_class(dp, "DecisionPoint")
  expect_equal(dp$id, "post_dropoff")
  expect_equal(dp$trigger, "dropoff")
  expect_null(dp$allowed_actions)
})

test_that("DecisionPoint: constructs with function trigger", {
  dp <- DecisionPoint("dp1", trigger = function(ev) ev$event_type == "X")
  expect_true(is.function(dp$trigger))
})

test_that("DecisionPoint: constructs with allowed_actions", {
  dp <- DecisionPoint("dp1", "dropoff", allowed_actions = c("charge", "idle"))
  expect_equal(dp$allowed_actions, c("charge", "idle"))
})

test_that("DecisionPoint: preserves the v2.0 positional observation and label slots", {
  observation_fn <- function(entity) list(x = entity$current$x)

  dp <- DecisionPoint(
    "dp1", "dropoff", "charge", NULL, NULL, TRUE,
    observation_fn, "Legacy positional call"
  )

  expect_identical(dp$observation_fn, observation_fn)
  expect_identical(dp$label, "Legacy positional call")
  expect_true(dp$audit)
  expect_identical(dp$on_pending_action, "warn")
})

test_that("DecisionPoint: named pending-action configuration remains available", {
  observation_fn <- function(entity) list()

  dp <- DecisionPoint(
    id = "dp1",
    trigger = "dropoff",
    observation_fn = observation_fn,
    label = "Named call",
    on_pending_action = "keep"
  )

  expect_identical(dp$observation_fn, observation_fn)
  expect_identical(dp$label, "Named call")
  expect_identical(dp$on_pending_action, "keep")
})

test_that("DecisionPoint: errors on empty id", {
  expect_error(DecisionPoint("", "dropoff"), "id")
})

test_that("DecisionPoint: errors on invalid trigger", {
  expect_error(DecisionPoint("dp1", trigger = 123), "trigger")
  expect_error(DecisionPoint("dp1", trigger = character(0)), "trigger")
})

test_that("DecisionPoint: errors on empty allowed_actions element", {
  expect_error(DecisionPoint("dp1", "ev", allowed_actions = c("ok", "")), "allowed_actions")
})

test_that("DecisionPoint: print method runs without error", {
  dp <- DecisionPoint("dp1", "ev", label = "test dp")
  expect_output(print(dp), "DecisionPoint")
})

# ---- dp_fires() -----------------------------------------------------------

test_that("dp_fires: fires when event_type matches character trigger", {
  dp <- DecisionPoint("dp1", trigger = c("dropoff", "pickup"))
  expect_true(dp_fires(dp, list(event_type = "dropoff")))
  expect_true(dp_fires(dp, list(event_type = "pickup")))
  expect_false(dp_fires(dp, list(event_type = "dispatch")))
})

test_that("dp_fires: fires when predicate function returns TRUE", {
  dp <- DecisionPoint("dp1", trigger = function(ev) ev$event_type == "X")
  expect_true(dp_fires(dp,  list(event_type = "X")))
  expect_false(dp_fires(dp, list(event_type = "Y")))
})

# ---- ActionEvent ----------------------------------------------------------

test_that("ActionEvent: constructs valid object", {
  ae <- ActionEvent("charge", time_next = 8.4, decision_point_id = "post_dropoff")
  expect_s3_class(ae, "ActionEvent")
  expect_equal(ae$action_type, "charge")
  expect_equal(ae$time_next, 8.4)
  expect_equal(ae$decision_point_id, "post_dropoff")
  expect_null(ae$params)
})

test_that("ActionEvent: accepts params and metadata", {
  ae <- ActionEvent("charge", 1.0, "dp1",
    params   = list(target_pct = 0.8),
    metadata = list(policy = "rule_based")
  )
  expect_equal(ae$params$target_pct, 0.8)
  expect_equal(ae$metadata$policy, "rule_based")
})

test_that("ActionEvent: errors on non-finite time_next", {
  expect_error(ActionEvent("charge", Inf,  "dp1"), "time_next")
  expect_error(ActionEvent("charge", NA,   "dp1"), "time_next")
  expect_error(ActionEvent("charge", "bad","dp1"), "time_next")
})

test_that("ActionEvent: errors on empty action_type", {
  expect_error(ActionEvent("", 1.0, "dp1"), "action_type")
})

test_that("ActionEvent: print method runs without error", {
  ae <- ActionEvent("idle", 5.0, "dp1")
  expect_output(print(ae), "ActionEvent")
})

# ---- state_summary_default ------------------------------------------------

test_that("state_summary_default: returns named list from entity$current", {
  e <- Entity$new(schema = list(
    x = list(type = "numeric", default = 1.5,
             validate = function(v) is.numeric(v))
  ))
  res <- state_summary_default(e)
  expect_true(is.list(res))
  expect_true("x" %in% names(res))
})

# ---- TrajectoryRecord -----------------------------------------------------

test_that("TrajectoryRecord: constructs with required fields", {
  ae <- ActionEvent("charge", 8.4, "dp1")
  tr <- TrajectoryRecord(
    run_id            = "r1",
    entity_id         = "e1",
    t                 = 8.4,
    decision_point_id = "dp1",
    observation       = list(battery_pct = 0.22),
    realized_event    = list(event_type = "dropoff", time_next = 8.4)
  )
  expect_s3_class(tr, "TrajectoryRecord")
  expect_equal(tr$t, 8.4)
  expect_null(tr$state_before)
  expect_null(tr$state_after)
  expect_null(tr$reward)
})

test_that("TrajectoryRecord: accepts optional fields", {
  ae <- ActionEvent("charge", 8.4, "dp1")
  tr <- TrajectoryRecord(
    run_id = "r1", entity_id = "e1", t = 8.4,
    decision_point_id = "dp1",
    observation = list(x = 1),
    realized_event = list(event_type = "dropoff"),
    candidate_actions = c("charge", "idle"),
    selected_action = ae,
    state_before = list(battery_pct = 0.5),
    state_after  = list(battery_pct = 0.8),
    reward = 1.0
  )
  expect_equal(tr$selected_action$action_type, "charge")
  expect_equal(tr$state_before$battery_pct, 0.5)
  expect_equal(tr$reward, 1.0)
})

test_that("TrajectoryRecord: errors on non-ActionEvent selected_action", {
  expect_error(
    TrajectoryRecord("r1", "e1", 1.0, "dp1", list(), list(),
                     selected_action = list(action_type = "charge")),
    "ActionEvent"
  )
})

test_that("TrajectoryRecord: print method runs without error", {
  tr <- TrajectoryRecord("r1", "e1", 1.0, "dp1", list(x = 1), list())
  expect_output(print(tr), "TrajectoryRecord")
})

# ---- load_model() ---------------------------------------------------------

test_that("load_model: returns Engine in v2 mode", {
  bundle <- test_model_bundle()
  schema <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  engine <- load_model(schema = schema, bundle = bundle)
  expect_true(inherits(engine, "Engine"))
  expect_true(engine$.v2_mode)
})

test_that("load_model: ctx= parameter is removed from Engine$run()", {
  bundle <- test_model_bundle()
  schema <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  engine <- load_model(schema = schema, bundle = bundle)
  e <- Entity$new(schema = default_entity_schema())
  # ctx= is no longer a valid parameter; should error

  expect_error(engine$run(e, ctx = list()), "unused argument")
})

test_that("load_model: Engine$new() direct path also rejects ctx= formal", {
  bundle <- test_model_bundle()
  engine <- Engine$new(bundle = bundle)
  expect_false(engine$.v2_mode)
  e <- Entity$new(schema = default_entity_schema())
  # ctx= is no longer a valid parameter anywhere
  expect_error(engine$run(e, ctx = list()), "unused argument")
})

test_that("load_model: errors if schema missing", {
  bundle <- test_model_bundle()
  expect_error(load_model(bundle = bundle), "schema")
})

test_that("load_model: errors if bundle missing", {
  schema <- list(time_spec = time_spec(unit = "years"), variables = list(), event_catalog = character(0))
  expect_error(load_model(schema = schema), "bundle")
})

test_that("load_model: errors if trajectory supplied without decision_points", {
  bundle <- test_model_bundle()
  schema <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
    # no decision_points
  )
  expect_error(
    load_model(schema = schema, bundle = bundle, trajectory = list(detail = "summary")),
    "decision_points"
  )
})

test_that("load_model: accepts trajectory when decision_points declared", {
  bundle <- test_model_bundle()
  dp <- DecisionPoint("dp1", trigger = "VISIT")
  schema <- list(
    variables        = default_entity_schema(),
    time_spec        = time_spec(unit = "years"),
    event_catalog    = bundle$event_catalog,
    decision_points  = list(dp)
  )
  engine <- load_model(schema = schema, bundle = bundle, trajectory = list(detail = "summary"))
  expect_true(engine$.v2_mode)
  expect_false(is.null(engine$.trajectory))
})

test_that("load_model: errors if environment is not EnvironmentContext", {
  bundle <- test_model_bundle()
  schema <- list(
    variables = default_entity_schema(),
    time_spec = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  expect_error(
    load_model(schema = schema, bundle = bundle, environment = list(signals = list())),
    "EnvironmentContext"
  )
})

test_that("load_model: accepts valid RuntimeContext", {
  bundle <- test_model_bundle()
  schema <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  rc <- RuntimeContext(seed = 42L)
  engine <- load_model(schema = schema, bundle = bundle, runtime = rc)
  expect_equal(engine$.runtime$seed, 42L)
})

test_that("load_model: errors on v1.x ctx formals in bundle callbacks", {
  # Create a bundle that still uses ctx= formals (should hard error)
  bundle <- test_model_bundle()
  bundle$observe <- function(entity, event, ctx = NULL) {
    data.frame(time = entity$last_time, stringsAsFactors = FALSE)
  }
  schema <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  expect_error(
    load_model(schema = schema, bundle = bundle),
    "ctx"
  )
})

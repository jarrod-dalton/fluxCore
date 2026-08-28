.q4_bundle <- function(time_spec, capture = NULL) {
  record_time <- function(sim_ctx) {
    if (!is.null(capture)) {
      capture$time_specs <- c(capture$time_specs, list(sim_ctx$time_spec))
    }
  }

  list(
    time_spec = time_spec,
    event_catalog = "tick",
    propose_events = function(entity, sim_ctx = NULL) {
      record_time(sim_ctx)
      if (entity$last_j > 0L) return(list())
      list(clock = list(time_next = 1, event_type = "tick"))
    },
    transition = function(entity, event, sim_ctx = NULL) {
      record_time(sim_ctx)
      list(x = entity$current$x + 1L)
    },
    stop = function(entity, event, sim_ctx = NULL) {
      record_time(sim_ctx)
      TRUE
    }
  )
}

.q4_vars <- function() {
  set_schema(vars = list(x = list(type = "count", default = 0L)))
}

test_that("load_model accepts semantically equal independent time specs", {
  schema_time <- time_spec(
    unit = "hours",
    origin = as.Date("2020-01-01"),
    zone = "America/New_York"
  )
  bundle_time <- time_spec(
    unit = "hours",
    origin = as.Date("2020-01-01"),
    zone = "America/New_York"
  )
  schema <- set_schema(vars = .q4_vars(), time_spec = schema_time)
  bundle <- .q4_bundle(bundle_time)

  engine <- expect_no_warning(load_model(schema, bundle))

  expect_s3_class(engine, "Engine")
  expect_identical(engine$time_spec, bundle_time)
  expect_false(".time_spec" %in% names(engine))
})

test_that("load_model rejects every time-spec mismatch before callbacks", {
  origin <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  base_time <- time_spec(
    unit = "hours",
    origin = origin,
    zone = "UTC"
  )
  mismatches <- list(
    unit = time_spec(
      unit = "days",
      origin = origin,
      zone = "UTC"
    ),
    origin_instant = time_spec(
      unit = "hours",
      origin = as.POSIXct("2020-01-02 00:00:00", tz = "UTC"),
      zone = "UTC"
    ),
    origin_class = time_spec(
      unit = "hours",
      origin = as.Date("2020-01-01"),
      zone = "UTC"
    ),
    zone = time_spec(
      unit = "hours",
      origin = origin,
      zone = "America/New_York"
    )
  )

  for (mismatch_name in names(mismatches)) {
    callback_calls <- 0L
    bundle <- .q4_bundle(mismatches[[mismatch_name]])
    bundle$propose_events <- function(entity, sim_ctx = NULL) {
      callback_calls <<- callback_calls + 1L
      list()
    }
    schema <- set_schema(vars = .q4_vars(), time_spec = base_time)

    expect_error(
      load_model(schema, bundle),
      "`schema\\$time_spec` and `bundle\\$time_spec` must be semantically equal"
    )
    expect_identical(callback_calls, 0L, info = mismatch_name)
  }
})

test_that("the accepted model clock reaches single and cohort SimContexts", {
  accepted_time <- time_spec(
    unit = "minutes",
    origin = as.POSIXct("2023-04-05 12:00:00", tz = "UTC"),
    zone = "UTC"
  )
  schema <- set_schema(
    vars = .q4_vars(),
    time_spec = time_spec(
      unit = "minutes",
      origin = as.POSIXct("2023-04-05 12:00:00", tz = "UTC"),
      zone = "UTC"
    )
  )
  capture <- new.env(parent = emptyenv())
  capture$time_specs <- list()
  engine <- load_model(schema, .q4_bundle(accepted_time, capture))

  single <- engine$run(Entity$new(schema = schema$variables, id = "single"))
  expect_identical(single$stopped_by, "stop")

  cohort <- run_cohort(
    engine,
    entities = list(
      first = Entity$new(schema = schema$variables, id = "first"),
      second = Entity$new(schema = schema$variables, id = "second")
    ),
    max_events = 1,
    backend = "none"
  )
  expect_length(cohort$runs, 2L)
  expect_true(length(capture$time_specs) >= 9L)
  expect_true(all(vapply(
    capture$time_specs,
    function(x) identical(x, engine$time_spec),
    logical(1)
  )))
})

test_that("variables-only load_model path warns once and uses the bundle clock", {
  variables <- .q4_vars()
  bundle_time <- time_spec(unit = "days")
  warnings <- character()

  engine <- withCallingHandlers(
    load_model(variables, .q4_bundle(bundle_time)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, "variables-only schema")
  expect_match(warnings, "2.1 compatibility", fixed = TRUE)
  expect_identical(engine$time_spec, bundle_time)
})

test_that("full schema without time_spec is malformed", {
  malformed <- list(
    variables = .q4_vars(),
    decision_points = list()
  )

  expect_error(
    load_model(malformed, .q4_bundle(time_spec(unit = "days"))),
    "full schema.*must define `schema\\$time_spec`"
  )
})

test_that("lower-level Engine and Entity constructors remain warning-free", {
  variables <- .q4_vars()
  bundle_time <- time_spec(unit = "weeks")

  engine <- expect_no_warning(Engine$new(.q4_bundle(bundle_time)))
  entity <- expect_no_warning(Entity$new(schema = variables))

  expect_identical(engine$time_spec, bundle_time)
  expect_s3_class(entity, "Entity")
})

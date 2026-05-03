## Stage 3 tests: trajectory logging emission and shape

.make_stage3_bundle <- function() {
  propose_events <- function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    pid <- "default"
    if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
    t0 <- entity$last_time
    list(default = list(time_next = t0 + 1, event_type = "VISIT", process_id = pid))
  }

  transition <- function(entity, event, ctx = NULL) {
    if (identical(event$event_type, "ACT")) return(list(acted = TRUE))
    list()
  }

  stop <- function(entity, event, ctx = NULL) {
    isTRUE(identical(event$event_type, "ACT"))
  }

  list(
    time_spec = time_spec(unit = "years"),
    event_catalog = c("VISIT", "ACT"),
    terminal_events = "ACT",
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    observe = NULL,
    refresh_rules = function(entity, last_event, changes, ctx = NULL) "ALL"
  )
}

.make_stage3_schema <- function(custom_observation = FALSE) {
  dp <- DecisionPoint(
    id = "after_visit",
    trigger = "VISIT",
    allowed_actions = "ACT",
    observation_fn = if (isTRUE(custom_observation)) {
      function(entity) list(marker = "ok", acted = entity$current$acted)
    } else {
      NULL
    }
  )

  list(
    variables = list(
      acted = list(
        type = "binary",
        levels = c("0", "1"),
        default = FALSE,
        coerce = as.logical,
        validate = function(x) length(x) == 1L && is.logical(x)
      )
    ),
    time_spec = time_spec(unit = "years"),
    event_catalog = c("VISIT", "ACT"),
    decision_points = list(dp)
  )
}

.make_stage3_policy <- function() {
  list(
    propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
      ActionEvent(
        action_type = "ACT",
        time_next = entity$last_time + 0.1,
        decision_point_id = decision_point$id
      )
    }
  )
}

test_that("Engine v2: returns trajectory_records when trajectory logger is configured", {
  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()

  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "summary")
  ))
  entity <- Entity$new(schema = schema$variables, id = "p1")

  out <- engine$run(entity = entity, max_events = 10)

  expect_true("trajectory_records" %in% names(out))
  expect_true(length(out$trajectory_records) >= 1L)
  expect_true(is.list(out$trajectory_records[[1]]))
  expect_equal(out$trajectory_records[[1]]$decision_point_id, "after_visit")
})

test_that("Engine v2: no trajectory_records field when trajectory logger is not configured", {
  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()

  engine <- suppressWarnings(load_model(schema = schema, bundle = bundle, policy = policy))
  entity <- Entity$new(schema = schema$variables)

  out <- engine$run(entity = entity, max_events = 10)
  expect_false("trajectory_records" %in% names(out))
})

test_that("Engine v2 trajectory detail=summary captures state_before/state_after", {
  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()

  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "summary")
  ))
  entity <- Entity$new(schema = schema$variables)

  out <- engine$run(entity = entity, max_events = 10)
  rec <- out$trajectory_records[[1]]

  expect_true(is.list(rec$state_before))
  expect_true(is.list(rec$state_after))
})

test_that("Engine v2 trajectory detail=none leaves state_before/state_after as NULL", {
  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()

  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "none")
  ))
  entity <- Entity$new(schema = schema$variables)

  out <- engine$run(entity = entity, max_events = 10)
  rec <- out$trajectory_records[[1]]

  expect_null(rec$state_before)
  expect_null(rec$state_after)
})

test_that("Engine v2 trajectory uses DecisionPoint observation_fn when provided", {
  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema(custom_observation = TRUE)
  policy <- .make_stage3_policy()

  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "summary")
  ))
  entity <- Entity$new(schema = schema$variables)

  out <- engine$run(entity = entity, max_events = 10)
  rec <- out$trajectory_records[[1]]

  expect_equal(rec$observation$marker, "ok")
  expect_true("acted" %in% names(rec$observation))
})

test_that("Engine v2 trajectory records are JSON-compatible", {
  skip_if_not_installed("jsonlite")

  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()

  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "summary")
  ))
  entity <- Entity$new(schema = schema$variables, id = "p-json")

  out <- engine$run(entity = entity, max_events = 10)

  expect_no_error(
    jsonlite::toJSON(out$trajectory_records, auto_unbox = TRUE, null = "null")
  )
})

test_that("load_model: trajectory detail must be one of none/summary/full", {
  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()

  expect_error(
    suppressWarnings(load_model(
      schema = schema,
      bundle = bundle,
      trajectory = list(detail = "verbose")
    )),
    "trajectory\\$detail"
  )
})

test_that("run_cohort v2: trajectory records are deterministic across serial and mclapply", {
  skip_if(.Platform$OS.type != "unix", "mclapply backend requires unix-like OS")

  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()
  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "summary")
  ))

  entities <- list(
    p1 = Entity$new(schema = schema$variables, id = "p1"),
    p2 = Entity$new(schema = schema$variables, id = "p2")
  )

  serial <- run_cohort(
    engine = engine,
    entities = entities,
    n_param_draws = 1,
    n_sims = 2,
    backend = "none",
    seed = 20260503L
  )
  parallel <- run_cohort(
    engine = engine,
    entities = entities,
    n_param_draws = 1,
    n_sims = 2,
    backend = "mclapply",
    n_workers = 2,
    seed = 20260503L
  )

  expect_equal(serial$index, parallel$index)
  expect_equal(names(serial$runs), names(parallel$runs))

  rec_signature <- function(rec) {
    list(
      dp = rec$decision_point_id,
      t = rec$t,
      evt = rec$realized_event$event_type,
      act = if (!is.null(rec$selected_action)) rec$selected_action$action_type else NULL
    )
  }

  for (rid in names(serial$runs)) {
    s_tr <- serial$runs[[rid]]$trajectory_records
    p_tr <- parallel$runs[[rid]]$trajectory_records

    expect_equal(length(s_tr), length(p_tr), label = paste("trajectory length", rid))

    s_sig <- lapply(s_tr, rec_signature)
    p_sig <- lapply(p_tr, rec_signature)
    expect_equal(s_sig, p_sig, label = paste("trajectory signature", rid))
  }
})

test_that("run_cohort v2: trajectory records are deterministic across serial and future backend", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  bundle <- .make_stage3_bundle()
  schema <- .make_stage3_schema()
  policy <- .make_stage3_policy()
  engine <- suppressWarnings(load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "summary")
  ))

  entities <- list(
    p1 = Entity$new(schema = schema$variables, id = "p1"),
    p2 = Entity$new(schema = schema$variables, id = "p2")
  )

  serial <- run_cohort(
    engine = engine,
    entities = entities,
    n_param_draws = 1,
    n_sims = 2,
    backend = "none",
    seed = 20260503L
  )
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  parallel <- run_cohort(
    engine = engine,
    entities = entities,
    n_param_draws = 1,
    n_sims = 2,
    backend = "future",
    seed = 20260503L
  )

  expect_equal(serial$index, parallel$index)
  expect_equal(names(serial$runs), names(parallel$runs))

  rec_signature <- function(rec) {
    list(
      dp = rec$decision_point_id,
      t = rec$t,
      evt = rec$realized_event$event_type,
      act = if (!is.null(rec$selected_action)) rec$selected_action$action_type else NULL
    )
  }

  for (rid in names(serial$runs)) {
    s_tr <- serial$runs[[rid]]$trajectory_records
    p_tr <- parallel$runs[[rid]]$trajectory_records

    expect_equal(length(s_tr), length(p_tr), label = paste("future trajectory length", rid))

    s_sig <- lapply(s_tr, rec_signature)
    p_sig <- lapply(p_tr, rec_signature)
    expect_equal(s_sig, p_sig, label = paste("future trajectory signature", rid))
  }
})

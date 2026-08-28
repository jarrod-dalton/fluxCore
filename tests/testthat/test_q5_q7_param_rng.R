.q57_variables <- function() {
  list(
    x = list(
      type = "nonnegative_integer",
      default = 0L,
      coerce = as.integer,
      validate = function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
      }
    )
  )
}

.q57_entity <- function(id = "courier_1") {
  Entity$new(schema = .q57_variables(), id = id)
}

.q57_stochastic_bundle <- function(default_offset = 0) {
  list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = "delivery",
    params = list(offset = default_offset),
    propose_events = function(entity, param_ctx = NULL) {
      list(delivery = list(
        time_next = entity$last_time + param_ctx$params$offset + stats::runif(1),
        event_type = "delivery"
      ))
    },
    transition = function(entity, event, param_ctx = NULL) {
      list(x = entity$current$x + 1L)
    },
    stop = function(entity, event, param_ctx = NULL) TRUE
  )
}

.q57_stochastic_engine <- function(seed = NULL,
                                   replicate_id = NULL,
                                   backend = "none",
                                   n_workers = NULL) {
  bundle <- .q57_stochastic_bundle()
  schema <- list(
    variables = .q57_variables(),
    time_spec = bundle$time_spec,
    event_catalog = bundle$event_catalog
  )
  load_model(
    schema = schema,
    bundle = bundle,
    runtime = RuntimeContext(
      seed = seed,
      replicate_id = replicate_id,
      backend = backend,
      n_workers = n_workers
    )
  )
}

.q57_event_times <- function(batch) {
  unname(vapply(
    batch$runs,
    function(run) tail(run$events$time, 1L),
    numeric(1)
  ))
}

.q57_event_histories <- function(batch) {
  unname(lapply(batch$runs, function(run) run$events))
}

test_that("ParamContext accepts only positive lossless integer ids", {
  expect_identical(ParamContext(5.0, list())$draw_id, 5L)

  invalid <- list(
    0L,
    -1L,
    1.5,
    Inf,
    NA_real_,
    as.double(.Machine$integer.max) + 1,
    "5",
    TRUE,
    c(1, 2)
  )
  for (id in invalid) {
    expect_error(
      ParamContext(id, list()),
      "positive, losslessly integer-valued"
    )
  }
})

test_that("run_cohort rejects malformed draw collections before simulation callbacks", {
  callback_count <- 0L
  bundle <- .q57_stochastic_bundle()
  bundle$propose_events <- function(entity, param_ctx = NULL) {
    callback_count <<- callback_count + 1L
    list(delivery = list(time_next = 1, event_type = "delivery"))
  }
  engine <- Engine$new(bundle = bundle)
  entity <- .q57_entity()
  valid <- ParamContext(1L, list(offset = 0))

  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = valid),
    "outer list"
  )
  expect_error(
    run_cohort(
      engine,
      list(courier_1 = entity),
      n_param_draws = 2,
      param_draws = list(valid)
    ),
    "exactly 2"
  )
  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = list(list(offset = 0))),
    "Bare parameter payload lists"
  )

  atomic_spoof <- structure(1L, class = "ParamContext")
  missing_field <- structure(
    list(draw_id = 2L, params = list(offset = 0)),
    class = "ParamContext"
  )
  missing_name <- ParamContext(2L, list(offset = 0))
  names(missing_name)[[2L]] <- NA_character_
  bad_params <- ParamContext(3L, list(offset = 0))
  bad_params$params <- "not-a-list"
  bad_id <- ParamContext(4L, list(offset = 0))
  bad_id$draw_id <- 4.5

  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = list(atomic_spoof)),
    "only ParamContext"
  )
  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = list(missing_field)),
    "malformed ParamContext"
  )
  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = list(missing_name)),
    "malformed ParamContext"
  )
  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = list(bad_params)),
    "malformed ParamContext"
  )
  expect_error(
    run_cohort(engine, list(courier_1 = entity), param_draws = list(bad_id)),
    "invalid ParamContext"
  )
  expect_error(
    run_cohort(
      engine,
      list(courier_1 = entity),
      n_param_draws = 2,
      param_draws = list(valid, ParamContext(1L, list(offset = 1)))
    ),
    "duplicated ParamContext draw id"
  )

  sampled_bundle <- bundle
  sampled_bundle$sample_params <- function(D) {
    lapply(seq_len(D), function(i) list(offset = i))
  }
  expect_error(
    run_cohort(
      Engine$new(bundle = sampled_bundle),
      list(courier_1 = entity),
      n_param_draws = 2
    ),
    "Bare parameter payload lists"
  )

  expect_identical(callback_count, 0L)
})

test_that("fallback, direct run, and run_draw paths expose one ParamContext", {
  seen <- new.env(parent = emptyenv())
  bundle <- .q57_stochastic_bundle(default_offset = 0.25)
  bundle$propose_events <- function(entity, param_ctx = NULL) {
    seen$param_ctx <- param_ctx
    list(delivery = list(time_next = entity$last_time + 1, event_type = "delivery"))
  }
  engine <- Engine$new(bundle = bundle)

  batch <- run_cohort(
    engine,
    list(courier_1 = .q57_entity()),
    n_param_draws = 2,
    backend = "none"
  )
  expect_true(all(vapply(batch$param_draws, inherits, logical(1), "ParamContext")))
  expect_identical(vapply(batch$param_draws, `[[`, integer(1), "draw_id"), 1:2)
  expect_identical(lapply(batch$param_draws, `[[`, "params"), rep(list(bundle$params), 2L))
  expect_s3_class(seen$param_ctx, "ParamContext")
  expect_false(inherits(seen$param_ctx$params, "ParamContext"))

  engine$run(.q57_entity("direct"))
  expect_identical(seen$param_ctx$draw_id, 1L)
  expect_identical(seen$param_ctx$params, bundle$params)

  engine$run_draw(
    .q57_entity("draw"),
    params = list(offset = 9),
    draw_id = 12L
  )
  expect_identical(seen$param_ctx$draw_id, 12L)
  expect_identical(seen$param_ctx$params, list(offset = 9))
})

test_that("cohort preserves one supplied ParamContext across supported callbacks", {
  seen <- new.env(parent = emptyenv())
  ts <- time_spec(unit = "hours")
  handler <- function(entity, event, param_ctx = NULL) {
    seen$handler <- param_ctx
    list(x = entity$current$x + 1L)
  }
  schema <- list(
    variables = .q57_variables(),
    time_spec = ts,
    event_catalog = c("tick", "dispatch"),
    decision_points = list(DecisionPoint(
      id = "after_tick",
      trigger = "tick",
      allowed_actions = "dispatch",
      action_handlers = list(dispatch = handler)
    ))
  )
  bundle <- list(
    time_spec = ts,
    event_catalog = c("tick", "dispatch"),
    propose_events = function(entity, param_ctx = NULL) {
      seen$proposal <- param_ctx
      if (entity$last_j == 0L) {
        return(list(tick = list(time_next = 1, event_type = "tick")))
      }
      list()
    },
    transition = function(entity, event, param_ctx = NULL) {
      seen$transition <- param_ctx
      NULL
    },
    stop = function(entity, event, param_ctx = NULL) {
      seen$stop <- param_ctx
      identical(event$event_type, "dispatch")
    }
  )
  policy <- list(propose_action = function(decision_point, entity, param_ctx = NULL) {
    seen$policy <- param_ctx
    ActionEvent("dispatch", time_next = entity$last_time + 1)
  })
  engine <- load_model(schema = schema, bundle = bundle, policy = policy)
  supplied <- ParamContext(
    draw_id = 42L,
    params = list(service_rate = 0.25),
    provenance = "posterior_row_42"
  )

  batch <- run_cohort(
    engine,
    list(courier_1 = .q57_entity()),
    param_draws = list(supplied),
    max_events = 5,
    backend = "none"
  )

  expect_identical(batch$param_draws[[1L]], supplied)
  expect_identical(batch$index$param_draw_id, 42L)
  for (callback in c("proposal", "transition", "stop", "policy", "handler")) {
    expect_identical(seen[[callback]], supplied, label = callback)
  }
})

test_that("stable draw ids determine canonical order and subset replay seeds", {
  engine <- Engine$new(bundle = .q57_stochastic_bundle())
  draws <- list(
    ParamContext(42L, list(offset = 42), "draw_42"),
    ParamContext(7L, list(offset = 7), "draw_7"),
    ParamContext(19L, list(offset = 19), "draw_19")
  )

  full <- run_cohort(
    engine,
    list(courier_1 = .q57_entity()),
    n_param_draws = 3,
    n_sims = 2,
    param_draws = draws,
    seed = 2468L,
    backend = "none"
  )
  expect_identical(
    vapply(full$param_draws, `[[`, integer(1), "draw_id"),
    c(7L, 19L, 42L)
  )
  expect_identical(full$index$param_draw_id, rep(c(7L, 19L, 42L), each = 2L))

  subset <- run_cohort(
    engine,
    list(courier_1 = .q57_entity()),
    n_param_draws = 1,
    n_sims = 2,
    param_draws = draws[1L],
    seed = 2468L,
    backend = "none"
  )
  expect_equal(
    .q57_event_times(full)[full$index$param_draw_id == 42L],
    .q57_event_times(subset)
  )
})

test_that("bundle sample_params contexts are canonical and remain direct", {
  seen <- new.env(parent = emptyenv())
  draws <- list(
    ParamContext(42L, list(offset = 4.2), "posterior_row_42"),
    ParamContext(7L, list(offset = 0.7), "posterior_row_7"),
    ParamContext(19L, list(offset = 1.9), "posterior_row_19")
  )
  bundle <- .q57_stochastic_bundle()
  bundle$sample_params <- function(D) {
    stopifnot(D == 3L)
    draws
  }
  bundle$propose_events <- function(entity, param_ctx = NULL) {
    seen[[as.character(param_ctx$draw_id)]] <- param_ctx
    list(delivery = list(
      time_next = entity$last_time + param_ctx$params$offset + stats::runif(1),
      event_type = "delivery"
    ))
  }

  batch <- run_cohort(
    Engine$new(bundle = bundle),
    list(courier_1 = .q57_entity()),
    n_param_draws = 3L,
    seed = 812L,
    backend = "none"
  )

  expected <- draws[c(2L, 3L, 1L)]
  expect_identical(batch$param_draws, expected)
  expect_identical(batch$index$param_draw_id, c(7L, 19L, 42L))
  expect_identical(
    vapply(batch$param_draws, `[[`, character(1), "provenance"),
    c("posterior_row_7", "posterior_row_19", "posterior_row_42")
  )
  for (context in expected) {
    expect_identical(seen[[as.character(context$draw_id)]], context)
    expect_false(inherits(seen[[as.character(context$draw_id)]]$params, "ParamContext"))
  }
})

test_that("seed allocation is safe at the maximum accepted draw id", {
  seed_a <- fluxCore:::.seed_for(
    base_seed = 42L,
    entity_id = "courier_1",
    param_draw_id = .Machine$integer.max,
    sim_id = 1L
  )
  seed_b <- fluxCore:::.seed_for(
    base_seed = 42L,
    entity_id = "courier_1",
    param_draw_id = .Machine$integer.max,
    sim_id = 1L
  )

  expect_type(seed_a, "integer")
  expect_false(is.na(seed_a))
  expect_gte(seed_a, 0L)
  expect_identical(seed_a, seed_b)
})

test_that("cohort RNG owner overrides stored Engine seed and is reproducible", {
  engine <- .q57_stochastic_engine(seed = 500L)
  entities <- list(courier_1 = .q57_entity())

  first <- run_cohort(
    engine,
    entities,
    n_sims = 4,
    seed = 900L,
    backend = "none"
  )
  second <- run_cohort(
    engine,
    entities,
    n_sims = 4,
    seed = 900L,
    backend = "none"
  )

  expect_equal(.q57_event_histories(first), .q57_event_histories(second))
  expect_gt(length(unique(.q57_event_times(first))), 1L)

  runtime_owned <- run_cohort(
    engine,
    entities,
    n_sims = 3,
    runtime = RuntimeContext(seed = 901L),
    seed = 999L,
    backend = "mclapply"
  )
  scalar_owned <- run_cohort(
    engine,
    entities,
    n_sims = 3,
    seed = 901L,
    backend = "none"
  )
  expect_equal(.q57_event_histories(runtime_owned), .q57_event_histories(scalar_owned))
})

test_that("cohort inherits stored runtime and explicit unseeded runtime overrides it", {
  engine <- .q57_stochastic_engine(seed = 500L, replicate_id = 77L)
  entities <- list(courier_1 = .q57_entity())

  inherited_a <- run_cohort(engine, entities, n_sims = 3)
  inherited_b <- run_cohort(engine, entities, n_sims = 3)
  expect_equal(.q57_event_histories(inherited_a), .q57_event_histories(inherited_b))
  expect_gt(length(unique(.q57_event_times(inherited_a))), 1L)

  set.seed(123L)
  expected <- stats::runif(2)
  set.seed(123L)
  unseeded <- run_cohort(
    engine,
    entities,
    n_sims = 2,
    runtime = RuntimeContext(seed = NULL)
  )
  expect_equal(.q57_event_times(unseeded), expected)

  callback_count <- 0L
  engine$bundle$propose_events <- function(entity, param_ctx = NULL) {
    callback_count <<- callback_count + 1L
    list(delivery = list(time_next = 1, event_type = "delivery"))
  }
  expect_error(
    run_cohort(
      engine,
      entities,
      runtime = RuntimeContext(seed = 1L, replicate_id = 2L)
    ),
    "replicate_id = NULL"
  )
  expect_identical(callback_count, 0L)
})

test_that("direct run retains stored seeding and run_draw preserves caller RNG", {
  engine <- .q57_stochastic_engine(seed = 500L, replicate_id = 3L)

  set.seed(1L)
  direct_a <- engine$run(.q57_entity())
  set.seed(999L)
  direct_b <- engine$run(.q57_entity())
  expect_equal(direct_a$events, direct_b$events)

  set.seed(321L)
  expected <- stats::runif(1)
  set.seed(321L)
  draw <- engine$run_draw(
    .q57_entity(),
    params = list(offset = 0),
    draw_id = 9L,
    sim_id = 4L
  )
  expect_equal(tail(draw$events$time, 1L), expected)
})

test_that("complete stochastic outputs agree across serial and mclapply", {
  skip_if(.Platform$OS.type != "unix", "mclapply backend requires unix-like OS")
  engine <- .q57_stochastic_engine(seed = 500L)
  entities <- list(
    courier_1 = .q57_entity("courier_1"),
    courier_2 = .q57_entity("courier_2")
  )
  draws <- list(
    ParamContext(19L, list(offset = 19)),
    ParamContext(7L, list(offset = 7))
  )
  args <- list(
    engine = engine,
    entities = entities,
    n_param_draws = 2,
    n_sims = 2,
    param_draws = draws,
    seed = 777L
  )

  serial <- do.call(run_cohort, c(args, list(backend = "none")))
  parallel <- do.call(run_cohort, c(args, list(backend = "mclapply", n_workers = 2L)))

  expect_identical(serial$index, parallel$index)
  expect_identical(serial$param_draws, parallel$param_draws)
  expect_equal(.q57_event_histories(serial), .q57_event_histories(parallel))
})

test_that("complete stochastic outputs agree through the future backend", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  engine <- .q57_stochastic_engine(seed = 500L)
  entities <- list(
    courier_1 = .q57_entity("courier_1"),
    courier_2 = .q57_entity("courier_2")
  )
  draws <- list(
    ParamContext(19L, list(offset = 19)),
    ParamContext(7L, list(offset = 7))
  )

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  serial <- run_cohort(
    engine,
    entities,
    n_param_draws = 2,
    n_sims = 2,
    param_draws = draws,
    seed = 777L,
    backend = "none"
  )
  expect_no_warning(
    future_out <- run_cohort(
      engine,
      entities,
      n_param_draws = 2,
      n_sims = 2,
      param_draws = rev(draws),
      seed = 777L,
      backend = "future"
    )
  )

  expect_identical(serial$index, future_out$index)
  expect_identical(serial$param_draws, future_out$param_draws)
  expect_equal(.q57_event_histories(serial), .q57_event_histories(future_out))
})

test_that("future RNG declaration follows the outer seed owner", {
  expect_null(fluxCore:::.future_seed_option(777L))
  expect_true(fluxCore:::.future_seed_option(NULL))

  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  engine <- .q57_stochastic_engine(seed = 500L)
  entities <- list(
    courier_1 = .q57_entity("courier_1"),
    courier_2 = .q57_entity("courier_2")
  )
  runtime <- RuntimeContext(seed = NULL, backend = "future")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  set.seed(90210L)
  expect_no_warning(
    first <- run_cohort(engine, entities, n_sims = 2L, runtime = runtime)
  )
  set.seed(90210L)
  expect_no_warning(
    second <- run_cohort(engine, entities, n_sims = 2L, runtime = runtime)
  )
  expect_equal(.q57_event_histories(first), .q57_event_histories(second))
  expect_gt(length(unique(.q57_event_times(first))), 1L)
})

test_that("seeded multisession future execution is warning-free and aligned", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  plan_result <- suppressWarnings(
    try(future::plan(future::multisession, workers = 2L), silent = TRUE)
  )
  if (inherits(plan_result, "try-error")) {
    skip("multisession workers unavailable in this environment")
  }
  probe <- try(future::value(future::future(TRUE)), silent = TRUE)
  if (inherits(probe, "try-error")) {
    skip("multisession workers unavailable in this environment")
  }

  engine <- .q57_stochastic_engine(seed = 500L)
  entities <- list(
    courier_1 = .q57_entity("courier_1"),
    courier_2 = .q57_entity("courier_2")
  )
  draws <- list(
    ParamContext(19L, list(offset = 19)),
    ParamContext(7L, list(offset = 7))
  )
  serial <- run_cohort(
    engine,
    entities,
    n_param_draws = 2L,
    n_sims = 2L,
    param_draws = draws,
    seed = 777L,
    backend = "none"
  )
  expect_no_warning(
    multisession <- run_cohort(
      engine,
      entities,
      n_param_draws = 2L,
      n_sims = 2L,
      param_draws = rev(draws),
      seed = 777L,
      backend = "future"
    )
  )

  expect_identical(serial$index, multisession$index)
  expect_identical(serial$param_draws, multisession$param_draws)
  expect_equal(.q57_event_histories(serial), .q57_event_histories(multisession))
})

test_that("deprecated Provider draw helpers retain the typed return contract", {
  bundle <- .q57_stochastic_bundle()
  provider <- fluxCore:::PackageProvider$new(registry = list(model = function() bundle))
  fallback <- provider$sample_param_draws(
    model_spec = list(name = "model"),
    n_param_draws = 2L
  )
  expect_true(all(vapply(fallback, inherits, logical(1), "ParamContext")))
  expect_identical(vapply(fallback, `[[`, integer(1), "draw_id"), 1:2)

  sampled <- bundle
  sampled$sample_params <- function(D) {
    list(
      ParamContext(42L, list(offset = 42)),
      ParamContext(7L, list(offset = 7))
    )
  }
  provider <- fluxCore:::PackageProvider$new(registry = list(model = function() sampled))
  ordered <- provider$sample_param_draws(
    model_spec = list(name = "model"),
    n_param_draws = 2L
  )
  expect_identical(vapply(ordered, `[[`, integer(1), "draw_id"), c(7L, 42L))

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(bundle, path)
  file_provider <- fluxCore:::FileProvider$new()
  expect_error(
    file_provider$sample_param_draws(
      model_spec = list(path = path),
      n_param_draws = 0L
    ),
    "positive integer"
  )
  file_draws <- file_provider$sample_param_draws(
    model_spec = list(path = path),
    n_param_draws = 2L
  )
  expect_true(all(vapply(file_draws, inherits, logical(1), "ParamContext")))
  expect_identical(vapply(file_draws, `[[`, integer(1), "draw_id"), 1:2)

  mlflow_fallback <- fluxCore:::MLflowProvider$new(
    builder_fn = function(...) bundle
  )
  mlflow_draws <- mlflow_fallback$sample_param_draws(
    model_spec = list(model_uri = "local-test"),
    n_param_draws = 2L
  )
  expect_true(all(vapply(mlflow_draws, inherits, logical(1), "ParamContext")))
  expect_identical(vapply(mlflow_draws, `[[`, integer(1), "draw_id"), 1:2)

  mlflow_sampled <- fluxCore:::MLflowProvider$new(
    builder_fn = function(...) bundle,
    sampler_fn = function(..., D) {
      stopifnot(D == 2L)
      list(
        ParamContext(42L, list(offset = 42), "mlflow_42"),
        ParamContext(7L, list(offset = 7), "mlflow_7")
      )
    }
  )
  mlflow_ordered <- mlflow_sampled$sample_param_draws(
    model_spec = list(model_uri = "local-test"),
    n_param_draws = 2L
  )
  expect_identical(vapply(mlflow_ordered, `[[`, integer(1), "draw_id"), c(7L, 42L))
})

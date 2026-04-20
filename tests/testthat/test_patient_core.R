test_that("Entity update increments j and records event", {
  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  p <- Entity$new(
    init = list(age = 50, miles_to_work = 10),
    schema = schema,
    time0 = 0
  )

  expect_identical(p$j, 0L)
  expect_identical(p$last_time, 0)

  p$update(time = 1.5, event_type = "VISIT", changes = list(age = 51))

  expect_identical(p$j, 1L)
  expect_identical(p$last_time, 1.5)
  expect_true(nrow(p$events) >= 1)
  expect_identical(tail(p$events$event_type, 1), "VISIT")
  expect_equal(p$current$age, 51)
})

test_that("changes=NULL produces a valid event but does not overwrite unchanged variables", {
  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  p <- Entity$new(
    init = list(age = 50, miles_to_work = 10),
    schema = schema,
    time0 = 0
  )

  p$update(time = 1, event_type = "NOOP", changes = NULL)

  expect_identical(p$j, 1L)
  expect_equal(p$current$age, 50)
  expect_equal(p$current$miles_to_work, 10)
})

test_that("Engine run returns events and entity; time is non-decreasing", {
  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  eng <- Engine$new(provider = PackageProvider$new(), model_spec = list(name = "default"))

  p <- Entity$new(
    init = list(age = 50, miles_to_work = 10),
    schema = schema,
    time0 = 0
  )

  out <- eng$run(
    p, ctx = list(time = list(unit = "years")),
    max_events = 50,
    return_observations = TRUE
  )

  expect_true(is.list(out))
  expect_true(is.data.frame(out$events))
  expect_true(inherits(out$entity, "Entity"))
  expect_true(nrow(out$events) >= 1)

  # non-decreasing times
  tt <- out$events$time
  expect_true(all(diff(tt) >= -1e-12))
})

test_that("merge_patches respects policy_wins and baseline_wins", {
  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  a <- list(x = 1, y = 2)
  b <- list(y = 99, z = 3)

  out1 <- merge_patches(a, b, merge = "policy_wins")
  expect_identical(out1$x, 1)
  expect_identical(out1$y, 99)
  expect_identical(out1$z, 3)

  out2 <- merge_patches(a, b, merge = "baseline_wins")
  expect_identical(out2$x, 1)
  expect_identical(out2$y, 2)
  expect_identical(out2$z, 3)

  expect_error(merge_patches(a, b, merge = "error_on_conflict"))
})

test_that("run_cohort produces index with entity_id, param_draw_id, sim_id and correct number of runs", {
  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  eng <- Engine$new(provider = PackageProvider$new(), model_spec = list(name = "default"))

  entities <- lapply(1:3, function(i) {
    Entity$new(init = list(age = 40 + i, miles_to_work = 8),
                schema = schema,
                time0 = 0)
  })
  names(entities) <- paste0("id", 1:3)

  batch <- run_cohort(
    engine = eng,
    entities = entities,
    time_unit = "years",
    n_param_draws = 2,
    n_sims = 3,
    max_events = 10,
    backend = "none",
    seed = 1
  )

  expect_true(is.data.frame(batch$index))
  expect_equal(nrow(batch$index), 3 * 2 * 3)
  expect_true(all(c("entity_id","param_draw_id","sim_id","run_id") %in% names(batch$index)))
  expect_equal(length(batch$runs), nrow(batch$index))
})

test_that("Engine stops immediately when bundle stop() returns TRUE (no events after terminal)", {
  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  # Create a tiny bundle that stops when it emits event_type == "STOP"
  bundle <- list(
    propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      pid <- "default"
      if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
      t0 <- entity$last_time
      ev <- if (entity$j == 0L) {
        list(time_next = t0 + 1, event_type = "GO")
      } else {
        list(time_next = t0 + 1, event_type = "STOP")
      }
      list(default = ev)
    },
    transition = function(entity, event, ctx = NULL) {
      NULL
    },
    stop = function(entity, event, ctx = NULL) {
      identical(event$event_type, "STOP")
    },
    observe = NULL,
    sample_params = function(D) rep(list(NULL), as.integer(D))
  )

  prov <- PackageProvider$new(registry = list(x = function() bundle))
  eng <- Engine$new(provider = prov, model_spec = list(name = "x"))

  p <- Entity$new(init = list(age = 50, miles_to_work = 10),
                   schema = schema,
                   time0 = 0)

  out <- eng$run(
    p, ctx = list(time = list(unit = "years")),
    max_events = 10,
    return_observations = FALSE
  )

  # Expect exactly init + GO + STOP (3 events)
  expect_equal(nrow(out$events), 3)
  expect_identical(tail(out$events$event_type, 1), "STOP")
})


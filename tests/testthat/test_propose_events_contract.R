test_that("propose_events must return a named list keyed by process_id", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    propose_events = function(entity, ctx = NULL, ...) {
      list(list(time_next = entity$last_time + 1, event_type = "visit"))
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) FALSE
  )

  eng <- Engine$new(bundle = bundle)
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng$run(entity = p, max_events = 1, return_observations = FALSE),
    "propose_events must return a"
  )
})

test_that("propose_events rejects duplicated process_id names", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    propose_events = function(entity, ctx = NULL, ...) {
      setNames(
        list(
          list(time_next = entity$last_time + 1, event_type = "visit"),
          list(time_next = entity$last_time + 2, event_type = "visit")
        ),
        c("stream", "stream")
      )
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) FALSE
  )

  eng <- Engine$new(bundle = bundle)
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng$run(entity = p, max_events = 1, return_observations = FALSE),
    "duplicated process_id names"
  )
})

test_that("event_type must be declared in bundle event_catalog when provided", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    event_catalog = c("visit"),
    propose_events = function(entity, ctx = NULL, ...) {
      list(default = list(time_next = entity$last_time + 1, event_type = "unknown"))
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) FALSE
  )

  eng <- Engine$new(bundle = bundle)
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng$run(entity = p, max_events = 1, return_observations = FALSE),
    "not declared in bundle\\$event_catalog"
  )
})

test_that("valid named proposals and event_catalog pass", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    event_catalog = c("visit"),
    propose_events = function(entity, ctx = NULL, ...) {
      list(default = list(time_next = entity$last_time + 1, event_type = "visit"))
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) entity$last_time >= 1
  )

  eng <- Engine$new(bundle = bundle)
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_no_error(eng$run(entity = p, max_events = 3, return_observations = FALSE))
})

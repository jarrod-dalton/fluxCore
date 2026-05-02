test_that("ModelBundle accepts valid event_catalog and terminal_events metadata", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    event_catalog = c("visit", "failure"),
    terminal_events = "failure",
    propose_events = function(entity, ctx = NULL, ...) {
      list(p = list(time_next = entity$last_time + 1, event_type = "visit"))
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) entity$last_time >= 1
  )

  provider <- list(load = function(model_spec = NULL, ...) bundle)
  expect_no_error(Engine$new(provider = provider))
})

test_that("ModelBundle rejects terminal_events labels not in event_catalog", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    event_catalog = c("visit"),
    terminal_events = c("death", "failure"),
    propose_events = function(entity, ctx = NULL, ...) {
      list(p = list(time_next = entity$last_time + 1, event_type = "visit"))
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) entity$last_time >= 1
  )

  provider <- list(load = function(model_spec = NULL, ...) bundle)
  expect_error(
    Engine$new(provider = provider),
    "bundle\\$terminal_events contains labels not in bundle\\$event_catalog"
  )
})

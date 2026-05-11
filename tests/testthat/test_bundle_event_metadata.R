test_that("ModelBundle accepts valid event_catalog and terminal_events metadata", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    event_catalog = c("visit", "failure"),
    terminal_events = "failure",
    propose_events = function(entity, ...) {
      list(p = list(time_next = entity$last_time + 1, event_type = "visit"))
    },
    transition = function(entity, event) NULL,
    stop = function(entity, event) entity$last_time >= 1
  )

  expect_no_error(Engine$new(bundle = bundle))
})

test_that("ModelBundle rejects terminal_events labels not in event_catalog", {
  bundle <- list(
    time_spec = time_spec(unit = "days"),
    event_catalog = c("visit"),
    terminal_events = c("death", "failure"),
    propose_events = function(entity, ...) {
      list(p = list(time_next = entity$last_time + 1, event_type = "visit"))
    },
    transition = function(entity, event) NULL,
    stop = function(entity, event) entity$last_time >= 1
  )

  expect_error(
    Engine$new(bundle = bundle),
    "bundle\\$terminal_events contains labels not in bundle\\$event_catalog"
  )
})

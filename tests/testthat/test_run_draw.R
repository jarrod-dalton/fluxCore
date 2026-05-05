test_that("Engine$run_draw() runs with explicit params", {

  set.seed(99)
  bundle <- test_model_bundle()
  schema <- default_entity_schema()
  entity <- Entity$new(
    init = list(alive = TRUE, active_followup = TRUE),
    schema = schema,
    entity_type = "patient",
    time0 = 0
  )

  eng <- Engine$new(bundle = bundle)
  out <- eng$run_draw(entity, params = list(rate = 0.5), draw_id = 2L, sim_id = 3L,
                      max_events = 20)

  expect_true(is.list(out))
  expect_true(nrow(out$events) >= 1L)
  expect_s3_class(out$entity, "Entity")
})

test_that("Engine$run_draw() defaults work (no params)", {
  set.seed(42)
  bundle <- test_model_bundle()
  schema <- default_entity_schema()
  entity <- Entity$new(
    init = list(alive = TRUE, active_followup = TRUE),
    schema = schema,
    entity_type = "patient",
    time0 = 0
  )

  eng <- Engine$new(bundle = bundle)
  out <- eng$run_draw(entity, max_events = 10)

  expect_true(is.list(out))
  expect_true(nrow(out$events) >= 1L)
})

test_that("Engine$run_draw() respects max_time", {
  set.seed(7)
  bundle <- test_model_bundle()
  schema <- default_entity_schema()
  entity <- Entity$new(
    init = list(alive = TRUE, active_followup = TRUE),
    schema = schema,
    entity_type = "patient",
    time0 = 0
  )

  eng <- Engine$new(bundle = bundle)
  out <- eng$run_draw(entity, max_time = 2.0, max_events = 500)

  # Most events should be within max_time; last event may exceed it (engine

  # stops AFTER processing the event that crosses the boundary).
  n_within <- sum(out$events$time <= 2.0)
  expect_true(n_within >= 1L)
  # But no event should be proposed far beyond max_time
  expect_true(max(out$events$time) < 2.0 + 10)
})

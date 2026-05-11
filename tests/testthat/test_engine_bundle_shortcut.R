test_that("Engine$new(bundle = ...) constructs a usable engine without a provider", {
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
  expect_identical(eng$bundle, bundle)
  expect_identical(eng$time_spec, bundle$time_spec)

  out <- eng$run(entity, max_events = 50, return_observations = TRUE)
  expect_true(nrow(out$events) >= 1L)
})

test_that("Engine$new() with no bundle errors with clear message", {
  expect_error(Engine$new(), "bundle")
})

test_that("Engine$new(bundle = ...) rejects an invalid bundle", {
  bad_bundle <- list(time_spec = time_spec(unit = "years"))  # no propose_events/transition
  expect_error(Engine$new(bundle = bad_bundle))
})

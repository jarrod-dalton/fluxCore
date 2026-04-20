test_that("Entity has a meta slot for bundle bookkeeping", {
  schema <- fluxCore::default_entity_schema()
  schema$x <- list(type = "continuous", default = 1, coerce = as.numeric)

  p <- fluxCore::new_entity(init = list(x = 2), schema = schema, time0 = 0)
  expect_true(is.list(p$meta))
  expect_length(p$meta, 0)

  # Bundles may store arbitrary bookkeeping here
  p$meta$refresh_clock <- list(last_refresh_time = 12)
  expect_equal(p$meta$refresh_clock$last_refresh_time, 12)

  # Meta should persist across updates
  p$update(time = 5, event_type = "lab", changes = list(x = 3))
  expect_equal(p$meta$refresh_clock$last_refresh_time, 12)

  # Meta is not a state variable
  st <- p$state()
  expect_false("meta" %in% names(st))
})

test_that("new_entity can attach entity_type metadata", {
  p <- fluxCore::new_entity(
    init = list(),
    schema = fluxCore::default_entity_schema(),
    entity_type = "entity"
  )
  expect_identical(p$meta$entity_type, "entity")
})

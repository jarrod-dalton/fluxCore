test_that("Entity$new requires explicit schema", {
  expect_error(
    Entity$new(init = list(x = 1)),
    "schema is required"
  )
})

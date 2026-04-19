test_that("Schema helpers validate and lookup metadata", {
  schema <- list(
    x = list(type = "continuous", default = 0, coerce = as.numeric),
    y = list(type = "categorical", levels = c("a", "b"), default = "a", coerce = as.character),
    z = list(type = "binary", levels = c("0", "1"), default = TRUE, coerce = as.logical, blocks = c("vitals"))
  )

  expect_silent(schema_validate(schema))
  expect_silent(schema_assert_vars(schema, c("x", "y")))
  expect_error(schema_assert_vars(schema, c("nope")), "Unknown schema variable", fixed = TRUE)

  info <- schema_var_info(schema, c("x", "y", "z"))
  expect_equal(info$type, c("continuous", "categorical", "binary"))
  expect_true(is.list(info$levels))
  expect_true(is.list(info$blocks))

  expect_silent(schema_assert_types(schema, c("x"), allowed_types = c("continuous")))
  expect_error(schema_assert_types(schema, c("x"), allowed_types = c("categorical")), "incompatible type", fixed = TRUE)

  expect_silent(schema_assert_levels(schema, "y", c("a")))
  expect_error(schema_assert_levels(schema, "y", c("c")), "not declared", fixed = TRUE)
  expect_error(schema_assert_levels(schema, "x", c("a")), "does not declare", fixed = TRUE)
})

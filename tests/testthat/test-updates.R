test_that("update_block validates block membership and schema", {
  schema <- default_patient_schema()
  schema$sbp <- list(default = 120, coerce = as.numeric, validate = NULL, blocks = c("bp"))
  schema$dbp <- list(default = 80,  coerce = as.numeric, validate = NULL, blocks = c("bp"))
  schema$age <- list(default = 50,  coerce = as.numeric, validate = NULL, blocks = c("demo"))
  p <- new_patient(schema = schema)

  expect_equal(update_block(p, "bp", c(sbp = 125, dbp = 87)), list(sbp = 125, dbp = 87))
  expect_error(update_block(p, "bp", c(sbp = 125)), "missing required")
  # var exists in schema but not in bp block
  expect_error(update_block(p, "bp", c(sbp = 125, age = 55, dbp = 87)), "not in that block")
  # var does not exist in schema
  expect_error(update_block(p, "bp", c(sbp = 125, junk = 1, dbp = 87)), "not in schema")
  expect_error(update_block(p, "nope", c(sbp = 125, dbp = 87)), "Unknown")
})

test_that("update_block unknown policy can drop unknown vars", {
  schema <- default_patient_schema()
  schema$sbp <- list(default = 120, coerce = as.numeric, validate = NULL, blocks = c("bp"))
  schema$dbp <- list(default = 80,  coerce = as.numeric, validate = NULL, blocks = c("bp"))
  p <- new_patient(schema = schema)

  # drop unknown var, still require all block vars
  expect_warning(
    out <- update_block(p, "bp", list(sbp = 125, dbp = 87, extra = 1), unknown = "drop_warn_always"),
    "will drop"
  )
  expect_equal(out, list(sbp = 125, dbp = 87))
})

test_that("combine_updates concatenates and disallows duplicates", {
  expect_equal(combine_updates(list(a = 1), list(b = 2)), list(a = 1, b = 2))
  expect_null(combine_updates(NULL, NULL))
  expect_error(combine_updates(list(a = 1), list(a = 2)), "Duplicate updates")
})

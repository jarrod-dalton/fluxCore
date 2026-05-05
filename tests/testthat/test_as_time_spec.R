test_that("as_time_spec: passes through time_spec objects unchanged", {
  ts <- time_spec(unit = "days", origin = as.Date("2020-01-01"), zone = "UTC")
  result <- as_time_spec(ts)
  expect_identical(result, ts)
})

test_that("as_time_spec: coerces list with unit only", {
  result <- as_time_spec(list(unit = "years"))
  expect_true(inherits(result, "time_spec"))
  expect_equal(result$unit, "years")
  expect_equal(result$zone, "UTC")
})

test_that("as_time_spec: coerces list with unit + origin + zone", {
  result <- as_time_spec(list(
    unit = "weeks",
    origin = as.Date("1970-01-01"),
    zone = "UTC"
  ))
  expect_true(inherits(result, "time_spec"))
  expect_equal(result$unit, "weeks")
  expect_equal(result$origin_date, as.Date("1970-01-01"))
})

test_that("as_time_spec: errors on list without unit", {
  expect_error(as_time_spec(list(origin = as.Date("2020-01-01"))), "unit")
})

test_that("as_time_spec: errors on non-coercible types", {
  expect_error(as_time_spec("days"), "cannot coerce")
  expect_error(as_time_spec(42), "cannot coerce")
  expect_error(as_time_spec(NULL), "cannot coerce")
})

test_that("as_time_spec: list with invalid unit propagates time_spec error", {
  expect_error(as_time_spec(list(unit = "fortnights")), "Unsupported")
})

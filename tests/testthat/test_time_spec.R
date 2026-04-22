test_that("time_spec requires unit", {
  ctx <- list(time = list())
  expect_error(time_spec(ctx = ctx), "unit must be a non-empty single string.", fixed = TRUE)
})

test_that("time_spec validates zone", {
  ctx <- list(time = list(unit = "days", zone = "Not/AZone"))
  expect_error(time_spec(ctx = ctx), "Invalid zone:", fixed = TRUE)
})

test_that("Date conversion respects unit constants", {
  ctx <- set_time_unit(unit = "weeks")
  spec <- time_spec(ctx = ctx)

  d0 <- as.Date("1970-01-01")
  d1 <- as.Date("1970-01-08") # 7 days later
  t0 <- time_to_model(d0, spec)
  t1 <- time_to_model(d1, spec)

  expect_equal(t0, 0)
  expect_equal(t1, 1)

  ctx_m <- set_time_unit(unit = "months")
  spec_m <- time_spec(ctx = ctx_m)
  d2 <- as.Date("1970-02-01") # 31 days later
  t2 <- time_to_model(d2, spec_m)
  expect_equal(t2, 31 / 30.4375)
})

test_that("POSIXct conversion works and round-trips", {
  ctx <- set_time_unit(unit = "hours", zone = "UTC")
  spec <- time_spec(ctx = ctx)

  x <- as.POSIXct("1970-01-01 02:00:00", tz = "UTC")
  t <- time_to_model(x, spec)
  expect_equal(t, 2)

  x2 <- time_from_model(t, spec, class = "POSIXct")
  expect_equal(x2, x)
})

test_that("Time-only inputs are rejected", {
  ctx <- set_time_unit(unit = "hours", zone = "UTC")
  spec <- time_spec(ctx = ctx)

  dt <- as.difftime(3600, units = "secs")
  expect_error(time_to_model(dt, spec), "Time-only", fixed = TRUE)
})

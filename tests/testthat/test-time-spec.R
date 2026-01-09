test_that("ps_time_spec requires ctx$time$unit", {
  ctx <- list(time = list())
  expect_error(ps_time_spec(ctx), "ctx$time$unit", fixed = TRUE)
})

test_that("ps_time_spec validates zone", {
  ctx <- list(time = list(unit = "days", zone = "Not/AZone"))
  expect_error(ps_time_spec(ctx), "Invalid ctx$time$zone", fixed = TRUE)
})

test_that("Date conversion respects unit constants", {
  ctx <- ps_set_time_unit(unit = "weeks")
  spec <- ps_time_spec(ctx)

  d0 <- as.Date("1970-01-01")
  d1 <- as.Date("1970-01-08") # 7 days later
  t0 <- ps_time_to_model(d0, spec)
  t1 <- ps_time_to_model(d1, spec)

  expect_equal(t0, 0)
  expect_equal(t1, 1)

  ctx_m <- ps_set_time_unit(unit = "months")
  spec_m <- ps_time_spec(ctx_m)
  d2 <- as.Date("1970-02-01") # 31 days later
  t2 <- ps_time_to_model(d2, spec_m)
  expect_equal(t2, 31 / 30.4375)
})

test_that("POSIXct conversion works and round-trips", {
  ctx <- ps_set_time_unit(unit = "hours", zone = "UTC")
  spec <- ps_time_spec(ctx)

  x <- as.POSIXct("1970-01-01 02:00:00", tz = "UTC")
  t <- ps_time_to_model(x, spec)
  expect_equal(t, 2)

  x2 <- ps_time_from_model(t, spec, class = "POSIXct")
  expect_equal(x2, x)
})

test_that("Time-only inputs are rejected", {
  ctx <- ps_set_time_unit(unit = "hours", zone = "UTC")
  spec <- ps_time_spec(ctx)

  dt <- as.difftime(3600, units = "secs")
  expect_error(ps_time_to_model(dt, spec), "Time-only", fixed = TRUE)
})

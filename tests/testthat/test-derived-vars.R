test_that("snapshot includes derived vars and respects lookback semantics", {
  schema <- default_patient_schema()
  
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  schema$sbp <- list(type = "continuous", default = 120, coerce = as.numeric)
p <- Patient$new(
    init = list(age = 50, miles_to_work = 10),
    schema = schema,
    derived_vars = list(
      hosp_12 = derive("hosp_12", target = event("HOSP"), lookback_t = 12, fn = "count", include_current = TRUE, force = TRUE)
    ),
    time0 = 0
  )

  p$update(time = 1, event_type = "VISIT", changes = list(age = 51))
  p$update(time = 2, event_type = "HOSP", changes = NULL)
  p$update(time = 20, event_type = "HOSP", changes = NULL)

  s_now <- p$snapshot()
  expect_true("hosp_12" %in% names(s_now))
  expect_equal(s_now[["hosp_12"]], 1L)

  s_at2 <- p$snapshot_at(2)
  expect_equal(s_at2[["hosp_12"]], 1L)

  p2 <- Patient$new(
    init = list(age = 50, miles_to_work = 10),
    schema = schema,
    derived_vars = list(
      hosp_12 = derive("hosp_12", target = event("HOSP"), lookback_t = 12, fn = "count", include_current = FALSE, force = TRUE)
    ),
    time0 = 0
  )
  p2$update(time = 2, event_type = "HOSP", changes = NULL)
  expect_equal(p2$snapshot()[["hosp_12"]], 0L)
})

test_that("lag_of extracts k-th prior value from sparse variable history", {
  schema <- default_patient_schema()
  if (!"sbp" %in% names(schema)) {
    schema$sbp <- list(default = NA_real_, coerce = as.numeric, validate = function(x) TRUE)
  }

  p <- Patient$new(
    init = list(age = 50, miles_to_work = 10, sbp = 120),
    schema = schema,
    derived_vars = list(
      sbp_lag1 = lag_of("sbp_lag1", target = var("sbp"), k = 1, include_current = FALSE, force = FALSE),
      sbp_lag2 = lag_of("sbp_lag2", target = var("sbp"), k = 2, include_current = FALSE, force = TRUE, na_value = NA_real_)
    ),
    time0 = 0
  )

  p$update(time = 1, event_type = "VISIT", changes = list(sbp = 130))
  p$update(time = 2, event_type = "VISIT", changes = list(sbp = 140))

  expect_equal(p$snapshot()[["sbp_lag1"]], 130)
  expect_equal(p$snapshot()[["sbp_lag2"]], 120)

  p0 <- Patient$new(
    init = list(age = 50, miles_to_work = 10, sbp = 120),
    schema = schema,
    derived_vars = list(
      sbp_lag1 = lag_of("sbp_lag1", var("sbp"), k = 1, include_current = FALSE, force = FALSE)
    ),
    time0 = 0
  )
  expect_false("sbp_lag1" %in% names(p0$snapshot()))
})

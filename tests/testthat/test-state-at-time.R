test_that("state_at_time returns state at latest event time <= t and errors for t < time0", {
  schema <- default_patient_schema()
  
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
p <- Patient$new(init = list(age = 50, miles_to_work = 10), schema = schema, time0 = 0)

  p$update(time = 1, event_type = "VISIT", changes = list(age = 51))
  p$update(time = 3, event_type = "VISIT", changes = list(age = 53))

  s0 <- p$state_at_time(0.5)
  expect_equal(s0[["age"]], 50)

  s1 <- p$state_at_time(1.0)
  expect_equal(s1[["age"]], 51)

  s2 <- p$state_at_time(2.0)
  expect_equal(s2[["age"]], 51)

  expect_error(p$state_at_time(-0.1))
})

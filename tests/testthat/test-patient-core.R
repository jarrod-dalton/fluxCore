test_that("Patient update increments j and records event", {
  p <- Patient$new(
    init = list(age = 50, miles_to_work = 10),
    schema = default_patient_schema(),
    time0 = 0
  )

  expect_identical(p$j, 0L)
  expect_identical(p$last_time, 0)

  p$update(time = 1.5, event_type = "VISIT", changes = list(age = 51))

  expect_identical(p$j, 1L)
  expect_identical(p$last_time, 1.5)
  expect_true(nrow(p$events) >= 1)
  expect_identical(tail(p$events$event_type, 1), "VISIT")
  expect_equal(p$current$age, 51)
})

test_that("changes=NULL produces a valid event but does not overwrite unchanged variables", {
  p <- Patient$new(
    init = list(age = 50, miles_to_work = 10),
    schema = default_patient_schema(),
    time0 = 0
  )

  p$update(time = 1, event_type = "NOOP", changes = NULL)

  expect_identical(p$j, 1L)
  expect_equal(p$current$age, 50)
  expect_equal(p$current$miles_to_work, 10)
})

test_that("Engine run returns events and patient; time is non-decreasing", {
  eng <- Engine$new(provider = PackageProvider$new(), model_spec = list(name = "default"))

  p <- Patient$new(
    init = list(age = 50, miles_to_work = 10),
    schema = default_patient_schema(),
    time0 = 0
  )

  out <- eng$run(p, max_events = 50, return_observations = TRUE)

  expect_true(is.list(out))
  expect_true(is.data.frame(out$events))
  expect_true(inherits(out$patient, "Patient"))
  expect_true(nrow(out$events) >= 1)

  # non-decreasing times
  tt <- out$events$time
  expect_true(all(diff(tt) >= -1e-12))
})

test_that("merge_patches respects policy_wins and baseline_wins", {
  a <- list(x = 1, y = 2)
  b <- list(y = 99, z = 3)

  out1 <- merge_patches(a, b, merge = "policy_wins")
  expect_identical(out1$x, 1)
  expect_identical(out1$y, 99)
  expect_identical(out1$z, 3)

  out2 <- merge_patches(a, b, merge = "baseline_wins")
  expect_identical(out2$x, 1)
  expect_identical(out2$y, 2)
  expect_identical(out2$z, 3)

  expect_error(merge_patches(a, b, merge = "error_on_conflict"))
})

test_that("run_cohort produces index with patient_id, draw_id, sim_id and correct number of runs", {
  eng <- Engine$new(provider = PackageProvider$new(), model_spec = list(name = "default"))

  patients <- lapply(1:3, function(i) {
    Patient$new(init = list(age = 40 + i, miles_to_work = 8),
                schema = default_patient_schema(),
                time0 = 0)
  })
  names(patients) <- paste0("id", 1:3)

  batch <- run_cohort(
    engine = eng,
    patients = patients,
    n_param_draws = 2,
    n_sims = 3,
    max_events = 10,
    parallel = FALSE,
    seed = 1
  )

  expect_true(is.data.frame(batch$index))
  expect_equal(nrow(batch$index), 3 * 2 * 3)
  expect_true(all(c("patient_id","draw_id","sim_id","run_id") %in% names(batch$index)))
  expect_equal(length(batch$runs), nrow(batch$index))
})

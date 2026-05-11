test_that("run_cohort backend='cluster' runs and preserves index/run alignment", {
  skip_on_cran()

  if (!requireNamespace("parallel", quietly = TRUE)) {
    skip("parallel package not available")
  }
  cl_probe <- try(parallel::makeCluster(1), silent = TRUE)
  if (inherits(cl_probe, "try-error")) {
    skip("PSOCK cluster sockets unavailable in this environment")
  }
  parallel::stopCluster(cl_probe)

  schema <- default_entity_schema()
  schema$age <- list(type = "continuous", default = 40, coerce = as.numeric)
  schema$miles_to_work <- list(type = "continuous", default = 10, coerce = as.numeric)
  bundle <- list(
    time_spec = time_spec(unit = "years"),
    propose_events = function(entity, process_ids = NULL, current_proposals = NULL) {
      list(p = list(time_next = entity$last_time + 1, event_type = "tick"))
    },
    transition = function(entity, event) NULL,
    stop = function(entity, event) TRUE,
    observe = function(entity, event) {
      data.frame(time_unit = "years", stringsAsFactors = FALSE)
    }
  )
  eng <- Engine$new(bundle = bundle)

  entities <- lapply(1:3, function(i) {
    Entity$new(init = list(age = 40 + i, miles_to_work = 8),
                schema = schema,
                time0 = 0)
  })
  names(entities) <- paste0("id", 1:3)

  batch <- run_cohort(
    engine = eng,
    entities = entities,
    n_param_draws = 2,
    n_sims = 2,
    max_events = 5,
    backend = "cluster",
    n_workers = 2,
    seed = 1
  )

  expect_true(is.data.frame(batch$index))
  expect_equal(nrow(batch$index), 3 * 2 * 2)
  expect_equal(length(batch$runs), nrow(batch$index))

  # critical invariant: ordering of runs matches index rows
  expect_identical(names(batch$runs), batch$index$run_id)

  units <- vapply(batch$runs, function(z) z$observations$time_unit[[1]], character(1))
  expect_true(all(units == "years"))
})

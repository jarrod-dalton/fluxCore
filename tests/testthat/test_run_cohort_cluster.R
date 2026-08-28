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

  ts <- time_spec(unit = "years")
  variables <- list(x = list(type = "count", default = 0L))
  schema <- list(
    variables = variables,
    time_spec = ts,
    event_catalog = c("tick", "act"),
    decision_points = list(DecisionPoint(
      id = "after_tick",
      trigger = "tick",
      allowed_actions = "act"
    ))
  )
  bundle <- list(
    time_spec = ts,
    event_catalog = c("tick", "act"),
    propose_events = function(entity, process_ids = NULL, current_proposals = NULL,
                              sim_ctx = NULL) {
      list(p = list(
        time_next = entity$last_time + 1,
        event_type = "tick",
        callback_run_id = sim_ctx$run_id
      ))
    },
    transition = function(entity, event, sim_ctx = NULL) {
      if (!identical(event$callback_run_id, sim_ctx$run_id)) {
        base::stop("transition received inconsistent SimContext run_id")
      }
      NULL
    },
    stop = function(entity, event, sim_ctx = NULL) {
      if (!identical(event$callback_run_id, sim_ctx$run_id)) {
        base::stop("stop received inconsistent SimContext run_id")
      }
      TRUE
    },
    observe = function(entity, event) {
      data.frame(time_unit = "years", stringsAsFactors = FALSE)
    }
  )
  policy <- list(propose_action = function(decision_point, entity, sim_ctx = NULL) {
    ActionEvent(
      "act",
      time_next = entity$last_time + 0.1,
      params = list(callback_run_id = sim_ctx$run_id)
    )
  })
  eng <- load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "none")
  )

  entities <- lapply(1:3, function(i) {
    Entity$new(schema = variables, time0 = 0)
  })
  names(entities) <- paste0("id", 1:3)

  serial <- run_cohort(
    engine = eng,
    entities = entities,
    n_param_draws = 2,
    n_sims = 2,
    max_events = 5,
    backend = "none",
    seed = 1
  )
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
  expect_identical(batch$index, serial$index)
  expect_identical(names(batch$runs), names(serial$runs))

  units <- vapply(batch$runs, function(z) z$observations$time_unit[[1]], character(1))
  expect_true(all(units == "years"))

  trajectory_signature <- function(record) {
    list(
      run_id = record$run_id,
      entity_id = record$entity_id,
      proposal_callback_run_id = record$realized_event$callback_run_id,
      policy_callback_run_id = record$selected_action$params$callback_run_id
    )
  }
  for (run_id in names(batch$runs)) {
    index_row <- batch$index[batch$index$run_id == run_id, , drop = FALSE]
    cluster_records <- batch$runs[[run_id]]$trajectory_records
    serial_records <- serial$runs[[run_id]]$trajectory_records

    expect_true(all(vapply(
      cluster_records,
      function(record) identical(record$run_id, run_id),
      logical(1)
    )))
    expect_true(all(vapply(
      cluster_records,
      function(record) identical(record$entity_id, index_row$entity_id[[1]]),
      logical(1)
    )))
    expect_true(all(vapply(
      cluster_records,
      function(record) identical(record$realized_event$callback_run_id, run_id),
      logical(1)
    )))
    expect_true(all(vapply(
      cluster_records,
      function(record) identical(record$selected_action$params$callback_run_id, run_id),
      logical(1)
    )))
    expect_identical(
      lapply(cluster_records, trajectory_signature),
      lapply(serial_records, trajectory_signature)
    )
  }
})

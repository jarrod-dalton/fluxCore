test_that("run_cohort index is ordered entity -> draw -> sim", {
  schema <- default_entity_schema()
  # Use a minimal bundle that does not assume any non-core state vars.
  minimal_bundle <- list(
    propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      # Propose a single no-op event strictly after the current engine time.
      # This avoids triggering the engine error path "No proposals available"
      # while keeping the run behavior trivial for an ordering-only test.
      t0 <- if (is.null(entity$last_time)) 0 else as.numeric(entity$last_time)
      list(
        "000|noop" = list(time_next = t0 + 1, event_type = "noop")
      )
    },
    transition = function(entity, event, ctx = NULL) {
      # No state changes.
      list()
    },
    stop = function(entity, event, ctx = NULL) TRUE
  )
  minimal_provider <- list(load = function(model_spec = NULL, ...) minimal_bundle)
  # default_entity_schema() only defines core variables (alive, active_followup).
  # Use defaults for this ordering test; entity_id is supplied by the cohort runner.
  p1 <- new_entity(init = list(), schema = schema)
  p2 <- new_entity(init = list(), schema = schema)

    eng <- Engine$new(provider = minimal_provider)

  out <- run_cohort(
    engine = eng,
    entities = list(p1 = p1, p2 = p2),
    n_param_draws = 3,
    n_sims = 4,
    seed = 123,
    time_unit = "days"
  )

  idx <- out$index

  expect_identical(idx$entity_id, c(rep("p1", 12), rep("p2", 12)))

  # Within each entity: param_draw_id is nondecreasing.
  for (pid in unique(idx$entity_id)) {
    sub <- idx[idx$entity_id == pid, , drop = FALSE]
    expect_true(all(diff(sub$param_draw_id) >= 0))

    # Within each draw: sim_id increases 1..S in order.
    for (d in unique(sub$param_draw_id)) {
      subd <- sub[sub$param_draw_id == d, , drop = FALSE]
      expect_identical(subd$sim_id, seq_len(4))
    }
  }

  # Names(runs) match run_id ordering.
  expect_identical(names(out$runs), idx$run_id)
})

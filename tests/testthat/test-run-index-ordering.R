test_that("run_cohort index is ordered patient -> draw -> sim", {
  schema <- default_patient_schema()
  p1 <- new_patient(init = list(age = 50), schema = schema)
  p2 <- new_patient(init = list(age = 60), schema = schema)

  eng <- Engine$new()  # default provider + default bundle

  out <- run_cohort(
    engine = eng,
    patients = list(p1 = p1, p2 = p2),
    n_param_draws = 3,
    n_sims = 4,
    seed = 123
  )

  idx <- out$index

  expect_identical(idx$patient_id, c(rep("p1", 12), rep("p2", 12)))

  # Within each patient: draw_id is nondecreasing.
  for (pid in unique(idx$patient_id)) {
    sub <- idx[idx$patient_id == pid, , drop = FALSE]
    expect_true(all(diff(sub$draw_id) >= 0))

    # Within each draw: sim_id increases 1..S in order.
    for (d in unique(sub$draw_id)) {
      subd <- sub[sub$draw_id == d, , drop = FALSE]
      expect_identical(subd$sim_id, seq_len(4))
    }
  }

  # Names(runs) match run_id ordering.
  expect_identical(names(out$runs), idx$run_id)
})

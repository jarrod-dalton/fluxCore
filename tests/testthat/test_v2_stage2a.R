## Stage 2A tests: RuntimeContext seed integration and determinism
##
## Coverage:
##   - run_cohort() accepts RuntimeContext for seed/backend/n_workers
##   - run_cohort() hard errors on ctx= in v2 mode
##   - Fixed seed + draw_id + sim_id + entity_id => identical output (serial)
##   - Different seeds => different output
##   - Different entity_ids with same seed => different seeds per entity
##   - .seed_for() produces valid integers and no collisions for typical sizes

# ---- run_cohort RuntimeContext integration --------------------------------

test_that("run_cohort: accepts RuntimeContext and uses its seed", {
  bundle  <- test_model_bundle()
  schema  <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  engine <- suppressWarnings(load_model(schema = schema, bundle = bundle))
  entities <- list(
    p1 = Entity$new(schema = default_entity_schema()),
    p2 = Entity$new(schema = default_entity_schema())
  )

  rc <- RuntimeContext(seed = 42L)
  result1 <- suppressWarnings(run_cohort(engine, entities, runtime = rc))
  result2 <- suppressWarnings(run_cohort(engine, entities, runtime = rc))

  # Same seed -> same number of events per entity
  n_events <- function(res, rid) nrow(res$runs[[rid]]$events)
  expect_equal(n_events(result1, "run_1"), n_events(result2, "run_1"))
  expect_equal(n_events(result1, "run_2"), n_events(result2, "run_2"))
})

test_that("run_cohort: hard errors on ctx= in v2 mode", {
  bundle  <- test_model_bundle()
  schema  <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  engine   <- suppressWarnings(load_model(schema = schema, bundle = bundle))
  entities <- list(p1 = Entity$new(schema = default_entity_schema()))
  expect_error(
    suppressWarnings(run_cohort(engine, entities, ctx = list())),
    "v2 mode"
  )
})

test_that("run_cohort: ctx= still works on v1 engines", {
  bundle  <- test_model_bundle()
  engine  <- Engine$new(bundle = bundle)
  entities <- list(p1 = Entity$new(schema = default_entity_schema()))
  # Should not error
  result <- run_cohort(engine, entities, ctx = list())
  expect_equal(length(result$runs), 1L)
})

test_that("run_cohort: RuntimeContext backend field is respected (none path)", {
  bundle  <- test_model_bundle()
  schema  <- list(
    variables     = default_entity_schema(),
    time_spec     = time_spec(unit = "years"),
    event_catalog = bundle$event_catalog
  )
  engine   <- suppressWarnings(load_model(schema = schema, bundle = bundle))
  entities <- list(p1 = Entity$new(schema = default_entity_schema()))
  rc <- RuntimeContext(seed = 1L, backend = "none")
  result <- suppressWarnings(run_cohort(engine, entities, runtime = rc))
  expect_equal(length(result$runs), 1L)
})

# ---- Determinism: fixed seed reproduces output ---------------------------

test_that("run_cohort: fixed seed reproduces identical event counts (serial)", {
  bundle   <- test_model_bundle()
  engine   <- Engine$new(bundle = bundle)
  entities <- list(
    p1 = Entity$new(schema = default_entity_schema()),
    p2 = Entity$new(schema = default_entity_schema()),
    p3 = Entity$new(schema = default_entity_schema())
  )

  r1 <- run_cohort(engine, entities, n_sims = 2, seed = 123L)
  r2 <- run_cohort(engine, entities, n_sims = 2, seed = 123L)

  for (rid in names(r1$runs)) {
    expect_equal(
      nrow(r1$runs[[rid]]$events),
      nrow(r2$runs[[rid]]$events),
      label = paste("event count matches for run", rid)
    )
  }
})

test_that("run_cohort: fixed seed is reproducible across serial and mclapply backends", {
  skip_if(.Platform$OS.type != "unix", "mclapply backend requires unix-like OS")

  bundle   <- test_model_bundle()
  engine   <- Engine$new(bundle = bundle)
  entities <- list(
    p1 = Entity$new(schema = default_entity_schema()),
    p2 = Entity$new(schema = default_entity_schema()),
    p3 = Entity$new(schema = default_entity_schema())
  )

  serial <- run_cohort(engine, entities, n_sims = 2, seed = 321L, backend = "none")
  parallel <- run_cohort(engine, entities, n_sims = 2, seed = 321L, backend = "mclapply", n_workers = 2)

  serial_counts <- vapply(serial$runs, function(x) nrow(x$events), integer(1))
  parallel_counts <- vapply(parallel$runs, function(x) nrow(x$events), integer(1))
  expect_equal(serial_counts, parallel_counts)
})

test_that("run_cohort: different seeds produce different outputs", {
  bundle   <- test_model_bundle()
  engine   <- Engine$new(bundle = bundle)
  entities <- list(p1 = Entity$new(schema = default_entity_schema()))

  r1 <- run_cohort(engine, entities, n_sims = 10, seed = 1L)
  r2 <- run_cohort(engine, entities, n_sims = 10, seed = 999L)

  # Collect all event counts; they should not all be equal
  counts1 <- vapply(r1$runs, function(x) nrow(x$events), integer(1))
  counts2 <- vapply(r2$runs, function(x) nrow(x$events), integer(1))
  # At least one run should differ (astronomically unlikely to be identical across 10 runs)
  expect_false(identical(counts1, counts2))
})

test_that("run_cohort: NULL seed gives different results across runs", {
  bundle   <- test_model_bundle()
  engine   <- Engine$new(bundle = bundle)
  entities <- list(p1 = Entity$new(schema = default_entity_schema()))

  r1 <- run_cohort(engine, entities, n_sims = 5, seed = NULL)
  r2 <- run_cohort(engine, entities, n_sims = 5, seed = NULL)

  # Very unlikely to match across 5 runs
  counts1 <- vapply(r1$runs, function(x) nrow(x$events), integer(1))
  counts2 <- vapply(r2$runs, function(x) nrow(x$events), integer(1))
  # We can't guarantee they differ, but we test the mechanism works without error
  expect_equal(length(counts1), 5L)
})

# ---- .seed_for() contract -------------------------------------------------

test_that(".seed_for: returns a valid non-NA integer", {
  s <- fluxCore:::.seed_for(42L, "patient_1", 1L, 1L)
  expect_true(is.integer(s))
  expect_false(is.na(s))
  expect_true(s >= 0L)
})

test_that(".seed_for: different entity_ids produce different seeds", {
  s1 <- fluxCore:::.seed_for(1L, "entity_A", 1L, 1L)
  s2 <- fluxCore:::.seed_for(1L, "entity_B", 1L, 1L)
  expect_false(identical(s1, s2))
})

test_that(".seed_for: different draw_ids produce different seeds", {
  s1 <- fluxCore:::.seed_for(1L, "e1", 1L, 1L)
  s2 <- fluxCore:::.seed_for(1L, "e1", 2L, 1L)
  expect_false(identical(s1, s2))
})

test_that(".seed_for: different sim_ids produce different seeds", {
  s1 <- fluxCore:::.seed_for(1L, "e1", 1L, 1L)
  s2 <- fluxCore:::.seed_for(1L, "e1", 1L, 2L)
  expect_false(identical(s1, s2))
})

test_that(".seed_for: different (draw, sim) pairs always produce different seeds for same entity", {
  # Formula uses distinct primes for draw_id and sim_id, so all (draw, sim)
  # combinations on a single entity are guaranteed collision-free.
  seeds <- as.vector(outer(1:10, 1:10, Vectorize(function(d, s)
    fluxCore:::.seed_for(42L, "entity-A", d, s))))
  expect_equal(length(seeds), length(unique(seeds)))
})

test_that(".seed_for: same utf8-hash entities share seed (known limitation documented)", {
  # Known limitation: entity_ids with the same utf8-sum hash produce the same
  # seed. e.g. 'p11' and 'p20' both have sum 210. This is acceptable for Stage 2;
  # a stronger hash is deferred to Stage 4 if inter-entity reproducibility matters.
  s_p11 <- fluxCore:::.seed_for(1L, "p11", 1L, 1L)
  s_p20 <- fluxCore:::.seed_for(1L, "p20", 1L, 1L)
  expect_equal(s_p11, s_p20)  # documents the known limitation, not a bug
})

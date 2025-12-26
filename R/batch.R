# ------------------------------------------------------------------------------
# run_cohort()
#
# Purpose:
#   Run a simulation engine for multiple patients, optionally with repeated
#   parameter draws and repeated simulations per draw.
#
# Time units:
#   - This function accepts `time_unit` and places it into ctx$time_unit for each
#     run. Engine itself treats time as numeric; the unit is metadata for clarity.
#
# Returns:
#   A list containing run index metadata and results (patients, events, observations).
# ------------------------------------------------------------------------------

run_cohort <- function(engine,
                       patients,
                       n_param_draws = 1,
                       n_sims = 1,
                       param_draws = NULL,
                       max_events = 1000,
                       max_time = NULL,
                       return_observations = TRUE,
                       time_unit = NULL,
                        backend = NULL,
                        parallel = NULL,
                       n_workers = NULL,
                       seed = NULL) {

  n_param_draws <- as.integer(n_param_draws)
  n_sims <- as.integer(n_sims)
  if (!is.finite(n_param_draws) || n_param_draws < 1L) stop("n_param_draws must be a positive integer.")
  if (!is.finite(n_sims) || n_sims < 1L) stop("n_sims must be a positive integer.")

  if (!is.list(patients) || length(patients) == 0L) stop("patients must be a non-empty list of Patient objects.")

  # materialize global parameter draws
  if (is.null(param_draws)) {
    param_draws <- .maybe_sample_param_draws(engine, n_param_draws)
  } else {
    if (!is.list(param_draws) || length(param_draws) != n_param_draws) {
      stop("param_draws must be a list of length n_param_draws.")
    }
  }

  # Patient IDs (stable)
  patient_ids <- names(patients)
  if (is.null(patient_ids) || any(patient_ids == "")) {
    patient_ids <- paste0("p", seq_along(patients))
  }

  # Build run index (run_id -> patient/draw/sim)
  idx <- expand.grid(
    patient_id = patient_ids,
    draw_id = seq_len(n_param_draws),
    sim_id = seq_len(n_sims),
    stringsAsFactors = FALSE
  )
  idx$run_id <- paste0("run_", seq_len(nrow(idx)))

  # Group runs by patient for parallelization
  split_by_patient <- split(idx, idx$patient_id)

  # base seed
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (!is.finite(seed) || length(seed) != 1L) stop("seed must be an integer scalar.")
  }

  run_one_patient <- function(patient_id) {
    p0 <- patients[[patient_id]]
    if (is.null(p0)) stop("Unknown patient_id in patients list: ", patient_id)

    rows <- split_by_patient[[patient_id]]
    out_list <- vector("list", nrow(rows))

    for (r in seq_len(nrow(rows))) {
      draw_id <- rows$draw_id[[r]]
      sim_id  <- rows$sim_id[[r]]

      # Deep clone patient for each run (so sims don't interfere)
      p <- .clone_patient(p0)

      # Deterministic seeding per (patient, draw, sim) if seed is provided
      if (!is.null(seed)) {
        local_seed <- .seed_for(seed, patient_id, draw_id, sim_id)
        set.seed(local_seed)
      }

      ctx <- list(
        time_unit = time_unit,
        patient_id = patient_id,
        draw_id = draw_id,
        sim_id = sim_id,
        params = param_draws[[draw_id]]
      )

      if (!is.null(time_unit)) {
        ctx$time_unit <- time_unit
      }

      out <- engine$run(
        patient = p,
        max_events = max_events,
        max_time = max_time,
        return_observations = return_observations,
        ctx = ctx
      )

      out_list[[r]] <- out
    }

    out_list
  }

# Resolve backend. `parallel` is a deprecated alias:
# - parallel=TRUE  -> backend="cluster"
# - parallel=FALSE -> backend="none"
if (is.null(backend)) {
  if (!is.null(parallel)) {
    backend <- if (isTRUE(parallel)) "cluster" else "none"
  } else {
    backend <- "none"
  }
}
backend <- match.arg(backend, c("none", "cluster", "mclapply", "future"))

if (backend == "none") {
  patient_out <- lapply(names(split_by_patient), run_one_patient)
  runs <- do.call(c, patient_out)

} else if (backend == "cluster") {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("backend='cluster' requires the 'parallel' package (base R).", call. = FALSE)
  }
  if (is.null(n_workers)) {
    n_workers <- max(1L, parallel::detectCores() - 1L)
  }
  n_workers <- as.integer(n_workers)
  if (!is.finite(n_workers) || n_workers < 1L) stop("n_workers must be a positive integer.", call. = FALSE)

  cl <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(
    cl,
    varlist = c("patients", "split_by_patient", "engine", "param_draws", "max_events", "max_time",
                "return_observations", "seed", "run_one_patient",
                ".clone_patient", ".seed_for", ".maybe_sample_param_draws"),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, { library(patientSimCore) })

  patient_out <- parallel::parLapply(cl, names(split_by_patient), run_one_patient)
  runs <- do.call(c, patient_out)

} else if (backend == "mclapply") {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("backend='mclapply' requires the 'parallel' package (base R).", call. = FALSE)
  }
  if (.Platform$OS.type == "windows") {
    stop("backend='mclapply' is not supported on Windows. Use backend='cluster' or backend='future'.", call. = FALSE)
  }
  if (is.null(n_workers)) {
    n_workers <- max(1L, parallel::detectCores() - 1L)
  }
  n_workers <- as.integer(n_workers)
  if (!is.finite(n_workers) || n_workers < 1L) stop("n_workers must be a positive integer.", call. = FALSE)

  patient_out <- parallel::mclapply(names(split_by_patient), run_one_patient, mc.cores = n_workers)
  runs <- do.call(c, patient_out)

} else if (backend == "future") {
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("backend='future' requires the 'future.apply' package.", call. = FALSE)
  }
  patient_out <- future.apply::future_lapply(names(split_by_patient), run_one_patient)
  runs <- do.call(c, patient_out)
}
  # Ensure same order as idx rows (patient-major)
  runs_labeled <- vector("list", nrow(idx))
  for (k in seq_len(nrow(idx))) {
    runs_labeled[[k]] <- runs[[k]]
  }
  names(runs_labeled) <- idx$run_id

  list(runs = runs_labeled, index = idx, param_draws = param_draws)
}

# ---- internal helpers ----

.maybe_sample_param_draws <- function(engine, n_param_draws) {
  # 1) Bundle can provide sample_params(D)
  if (!is.null(engine$bundle$sample_params) && is.function(engine$bundle$sample_params)) {
    draws <- engine$bundle$sample_params(n_param_draws)
    if (!is.list(draws) || length(draws) != n_param_draws) {
      stop("bundle$sample_params(D) must return a list of length D.")
    }
    return(draws)
  }

  # 2) Provider can provide sample_param_draws(model_spec, D)
  if (!is.null(engine$provider) && !is.null(engine$provider$sample_param_draws) && is.function(engine$provider$sample_param_draws)) {
    draws <- engine$provider$sample_param_draws(model_spec = engine$model_spec, n_param_draws = n_param_draws)
    if (!is.list(draws) || length(draws) != n_param_draws) {
      stop("provider$sample_param_draws(model_spec, D) must return a list of length D.")
    }
    return(draws)
  }

  # Fallback: one NULL draw repeated
  rep(list(NULL), n_param_draws)
}

.clone_patient <- function(p) {
  # R6 deep clone: safe for per-sim runs
  p$clone(deep = TRUE)
}

.seed_for <- function(base_seed, patient_id, draw_id, sim_id) {
  # stable integer seed derived from identifiers
  # (simple; if you want stronger guarantees, switch to L'Ecuyer streams)
  h <- sum(utf8ToInt(as.character(patient_id))) %% 100000L
  as.integer((base_seed + 100000L * draw_id + 1000L * sim_id + h) %% .Machine$integer.max)
}

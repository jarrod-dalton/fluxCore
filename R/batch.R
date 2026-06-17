# ------------------------------------------------------------------------------
# run_cohort()
#
# Purpose:
#   Run a simulation engine for multiple entities, optionally with repeated
#   parameter draws and repeated simulations per draw.
#
# Time semantics:
#   - The canonical model time spec is declared once in bundle$time_spec.
#   - Runtime settings may not override time metadata.
#
# Returns:
#   A list containing run index metadata and results (entities, events, observations).
# ------------------------------------------------------------------------------

#' Run a cohort of entities (serial or parallel) with optional global parameter draws
#'
#' These helpers support batch simulation with:
#' global parameter draws reused across entities (parameter uncertainty)
#' multiple stochastic sims per entity per draw (stochastic uncertainty)
#' parallelization across entities
#'
#' @param engine An Engine object (with a materialized bundle).
#' @param entities List of Entity objects.
#' @param n_param_draws Integer; number of global parameter draws (D). Default 1.
#' @param n_sims Integer; number of stochastic sims per entity per draw (S). Default 1.
#' @param param_draws Optional; a list of length D with per-draw parameter contexts. If NULL,
#' the function will attempt to call engine$bundle$sample_params(D) when available;
#' otherwise it uses a single NULL draw (no global parameter variation).
#' @param runtime Optional [RuntimeContext]; carries seed, backend, and n_workers for v2-mode
#' engines. Takes precedence over the individual `seed`, `backend`, and `n_workers` arguments
#' when non-NULL.
#' @param max_events Max events per run.
#' @param max_time Optional max time per run.
#' @param return_observations Logical; whether to return observations (if bundle provides observe()).
#' @param backend Backend used to parallelize across entities. One of "none", "cluster",
#' "mclapply", or "future". Default is "none". Ignored when `runtime` is supplied.
#' @param n_workers Integer; workers for parallel; default parallel::detectCores() - 1.
#' Ignored when `runtime` is supplied.
#' @param seed Optional base seed for reproducibility. Public contract:
#' fixed `seed` + `draw_id` + `sim_id` + `entity_id` = reproducible output.
#' Ignored when `runtime` is supplied.
#'
#' @return
#' A list with:
#' runs: list of per-run outputs (entity/events/observations; plus trajectory_records when enabled) with labels
#' index: data.frame mapping run_id -> entity_id/param_draw_id/sim_id
#'
#' @export
run_cohort <- function(engine,
                       entities,
                       n_param_draws = 1,
                       n_sims = 1,
                       param_draws = NULL,
                       runtime = NULL,
                       max_events = 1000,
                       max_time = NULL,
                       return_observations = TRUE,
                       backend = NULL,
                       n_workers = NULL,
                       seed = NULL) {

  # v2 mode: RuntimeContext takes precedence
  if (!is.null(runtime)) {
    if (!inherits(runtime, "RuntimeContext")) {
      stop("run_cohort(): `runtime` must be a RuntimeContext or NULL.", call. = FALSE)
    }
    seed      <- runtime$seed
    backend   <- runtime$backend
    n_workers <- runtime$n_workers
  }

  n_param_draws <- as.integer(n_param_draws)
  n_sims <- as.integer(n_sims)
  if (!is.finite(n_param_draws) || n_param_draws < 1L) stop("n_param_draws must be a positive integer.")
  if (!is.finite(n_sims) || n_sims < 1L) stop("n_sims must be a positive integer.")

  if (!is.list(entities) || length(entities) == 0L) stop("entities must be a non-empty list of Entity objects.")
  if (is.null(engine$time_spec) || !inherits(engine$time_spec, "time_spec")) {
    stop("engine$time_spec must be a valid fluxCore `time_spec`.", call. = FALSE)
  }
  canonical_time_spec <- engine$time_spec

  # materialize global parameter draws
  if (is.null(param_draws)) {
    param_draws <- .maybe_sample_param_draws(engine, n_param_draws)
  } else {
    if (!is.list(param_draws) || length(param_draws) != n_param_draws) {
      stop("param_draws must be a list of length n_param_draws.")
    }
  }

  # Validate max_time early. This is intentionally strict to avoid accidental
  # partial-argument matches (e.g., passing `time = ...` which could match `max_time`).
  if (!is.null(max_time)) {
    if (!is.numeric(max_time) || length(max_time) != 1L || is.na(max_time) || !is.finite(max_time)) {
      stop("max_time must be a single finite numeric value or NULL.", call. = FALSE)
    }
  }

  # Entity IDs (stable)
  entity_ids <- names(entities)
  if (is.null(entity_ids) || any(entity_ids == "")) {
    entity_ids <- paste0("p", seq_along(entities))
    names(entities) <- entity_ids
  }

  # Build run index (run_id -> entity/draw/sim).
  #
  # IMPORTANT INVARIANT:
  #   The ordering of `idx` rows MUST match the ordering of the returned `runs`
  #   list (runs[[i]] corresponds to idx[i,]). Downstream packages (Forecast,
  #   Validation, orchestration) rely on this invariant for correct grouping.
  #
  # We therefore construct idx in entity-major order (entity -> draw -> sim),
  # which matches the natural execution/parallelization strategy used below.
  idx_list <- lapply(entity_ids, function(pid) {
    grid <- expand.grid(
      sim_id = seq_len(n_sims),
      param_draw_id = seq_len(n_param_draws),
      stringsAsFactors = FALSE
    )
    data.frame(
      entity_id = rep(pid, nrow(grid)),
      param_draw_id = grid$param_draw_id,
      sim_id = grid$sim_id,
      stringsAsFactors = FALSE
    )
  })
  idx <- do.call(rbind, idx_list)
  idx$run_id <- paste0("run_", seq_len(nrow(idx)))

  # Group runs by entity for parallelization (preserve entity_ids order)
  split_by_entity <- stats::setNames(lapply(entity_ids, function(pid) idx[idx$entity_id == pid, , drop = FALSE]),
                                     entity_ids)

  # base seed
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (!is.finite(seed) || length(seed) != 1L) stop("seed must be an integer scalar.")
  }

  run_one_entity <- function(entity_id) {
    .run_one_entity_worker(
      entity_id = entity_id,
      entities = entities,
      split_by_entity = split_by_entity,
      engine = engine,
      canonical_time_spec = canonical_time_spec,
      param_draws = param_draws,
      max_events = max_events,
      max_time = max_time,
      return_observations = return_observations,
      seed = seed
    )
  }

  # Resolve backend.
  if (is.null(backend)) backend <- "none"
  backend <- match.arg(backend, c("none", "cluster", "mclapply", "future"))

  if (backend == "none") {
    entity_out <- lapply(names(split_by_entity), run_one_entity)
    runs <- do.call(c, entity_out)

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

    parallel::clusterEvalQ(cl, { library(fluxCore); NULL })
    parallel::clusterExport(
      cl,
      varlist = c(".run_one_entity_worker", ".seed_for"),
      envir = asNamespace("fluxCore")
    )

    entity_out <- parallel::parLapply(
      cl,
      names(split_by_entity),
      .run_one_entity_worker,
      entities = entities,
      split_by_entity = split_by_entity,
      engine = engine,
      canonical_time_spec = canonical_time_spec,
      param_draws = param_draws,
      max_events = max_events,
      max_time = max_time,
      return_observations = return_observations,
      seed = seed
    )
    runs <- do.call(c, entity_out)

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

    entity_out <- parallel::mclapply(names(split_by_entity), run_one_entity, mc.cores = n_workers)
    runs <- do.call(c, entity_out)

  } else if (backend == "future") {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("backend='future' requires the 'future.apply' package.", call. = FALSE)
    }
    entity_out <- future.apply::future_lapply(names(split_by_entity), run_one_entity)
    runs <- do.call(c, entity_out)
  }

  # Label runs with run_id (ordering already matches idx by construction).
  if (length(runs) != nrow(idx)) {
    stop("Internal error: number of runs does not match run index rows.", call. = FALSE)
  }
  names(runs) <- idx$run_id

  list(runs = runs, index = idx, param_draws = param_draws)
}

# ---- internal helpers ----
.run_one_entity_worker <- function(entity_id,
                                   entities,
                                   split_by_entity,
                                   engine,
                                   canonical_time_spec,
                                   param_draws,
                                   max_events,
                                   max_time,
                                   return_observations,
                                   seed) {
  p0 <- entities[[entity_id]]
  if (is.null(p0)) stop("Unknown entity_id in entities list: ", entity_id)

  rows <- split_by_entity[[entity_id]]
  out_list <- vector("list", nrow(rows))

  for (r in seq_len(nrow(rows))) {
    param_draw_id <- rows$param_draw_id[[r]]
    sim_id <- rows$sim_id[[r]]

    # Deep clone entity for each run (so sims don't interfere)
    p <- p0$clone(deep = TRUE)

    # Deterministic seeding per (entity, draw, sim) if seed is provided
    if (!is.null(seed)) {
      local_seed <- .seed_for(seed, entity_id, param_draw_id, sim_id)
      set.seed(local_seed)
    }

    run_meta <- list(
      time_spec = canonical_time_spec,
      entity_id = entity_id,
      param_draw_id = param_draw_id,
      sim_id = sim_id,
      params = param_draws[[param_draw_id]]
    )

    out <- engine$run(
      entity = p,
      max_events = max_events,
      max_time = max_time,
      return_observations = return_observations,
      .internal_ctx = run_meta
    )

    out_list[[r]] <- out
  }

  out_list
}

.maybe_sample_param_draws <- function(engine, n_param_draws) {
  # v2.0: Use bundle$sample_params(D) to draw parameters.
  # Removed fallback to engine$provider (v1.x pattern).
  if (!is.null(engine$bundle$sample_params) && is.function(engine$bundle$sample_params)) {
    draws <- engine$bundle$sample_params(n_param_draws)
    if (!is.list(draws) || length(draws) != n_param_draws) {
      stop("bundle$sample_params(D) must return a list of length D.")
    }
    return(draws)
  }

  # Fallback: one NULL draw repeated (no global parameter variation)
  rep(list(NULL), n_param_draws)
}

.seed_for <- function(base_seed, entity_id, param_draw_id, sim_id) {
  # Deterministic stream seed derived from run coordinates.
  # Formula: base_seed + draw_id * P1 + sim_id * P2 + entity_hash * P3 (mod max_int)
  # P1/P2/P3 are distinct large primes chosen to minimise collisions across
  # typical cohort sizes (<=1e4 entities, <=1e3 draws, <=100 sims).
  # This is an INTERNAL detail. The public contract is:
  #   fixed seed + draw_id + sim_id + entity_id => reproducible output.
  # Stream allocation details must not appear in user-facing documentation.
  P1 <- 99991L   # prime ~1e5
  P2 <-  9973L   # prime ~1e4
  P3 <-   997L   # prime ~1e3
  h  <- sum(utf8ToInt(as.character(entity_id))) %% 100000L
  as.integer((base_seed + P1 * param_draw_id + P2 * sim_id + P3 * h) %% .Machine$integer.max)
}


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
#' @param param_draws Optional list of exactly `n_param_draws` [ParamContext]
#'   objects. Draw ids are stable identities: they must be positive and unique,
#'   need not be contiguous, and determine canonical cohort order. Bare parameter
#'   payload lists are not accepted. If `NULL`, the function calls
#'   `engine$bundle$sample_params(D)` when available; otherwise it constructs
#'   contexts numbered `1:D` from `bundle$params` or an empty parameter list.
#' @param runtime Optional [RuntimeContext]; carries seed, backend, and n_workers for v2-mode
#' engines. Takes precedence over the individual `seed`, `backend`, and `n_workers` arguments
#' when non-NULL. Its `replicate_id` must be `NULL`; cohort replicates are
#' represented by `sim_id`. An explicit runtime with `seed = NULL` requests an
#' unseeded cohort even when the Engine stores a seed.
#' @param max_events Max events per run.
#' @param max_time Optional max time per run.
#' @param return_observations Logical; whether to return observations (if bundle provides observe()).
#' @param backend Backend used to parallelize across entities. One of "none", "cluster",
#' "mclapply", or "future". When `NULL`, inherits the Engine's stored runtime
#' backend when available, then defaults to "none". Ignored when `runtime` is supplied.
#' @param n_workers Integer; workers for parallel; default parallel::detectCores() - 1.
#' When `NULL`, inherits the Engine's stored runtime setting when available.
#' Ignored when `runtime` is supplied.
#' @param seed Optional base seed for reproducibility. Public contract:
#' fixed `seed` + `draw_id` + `sim_id` + `entity_id` = reproducible output.
#' A non-`NULL` value overrides the Engine's stored runtime seed. Ignored when
#' `runtime` is supplied.
#'
#' @return
#' A list with:
#' runs: list of per-run outputs (entity/events/observations; plus trajectory_records when enabled) with labels
#' index: data.frame mapping run_id -> entity_id/param_draw_id/sim_id
#' param_draws: the validated contexts in ascending draw-id order
#'
#' The index `run_id` is a batch-local join key shared by the corresponding run,
#' its `SimContext`, and its trajectory records. For replay across cohort calls,
#' use the stable entity/draw/simulation coordinates rather than assuming that a
#' sequential `run_id` will remain unchanged when the cohort shape changes.
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

  # Resolve cohort settings once. The explicit RuntimeContext is authoritative;
  # otherwise non-NULL scalar controls override stored Engine defaults. The
  # Engine receives a private ownership marker for each run and must not seed a
  # second time.
  if (!is.null(runtime)) {
    if (!inherits(runtime, "RuntimeContext")) {
      stop("run_cohort(): `runtime` must be a RuntimeContext or NULL.", call. = FALSE)
    }
    if (!is.null(runtime$replicate_id)) {
      stop(
        "run_cohort(): an explicitly supplied RuntimeContext must have `replicate_id = NULL`; cohort replicates use `sim_id`.",
        call. = FALSE
      )
    }
    seed      <- runtime$seed
    backend   <- runtime$backend
    n_workers <- runtime$n_workers
  } else {
    stored_runtime <- engine$.runtime
    if (!is.null(stored_runtime) && inherits(stored_runtime, "RuntimeContext")) {
      if (is.null(seed)) seed <- stored_runtime$seed
      if (is.null(backend)) backend <- stored_runtime$backend
      if (is.null(n_workers)) n_workers <- stored_runtime$n_workers
    }
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

  # Materialize and normalize global parameter draws exactly once. Validation
  # occurs before the run grid is built or any simulation callback/worker starts.
  if (is.null(param_draws)) {
    draw_source <- if (!is.null(engine$bundle$sample_params) &&
                       is.function(engine$bundle$sample_params)) {
      "bundle$sample_params(D)"
    } else {
      "fluxCore fallback parameter draws"
    }
    param_draws <- .maybe_sample_param_draws(engine, n_param_draws)
  } else {
    draw_source <- "run_cohort() `param_draws`"
  }
  param_draws <- .normalize_param_draws(param_draws, n_param_draws, draw_source)
  draw_ids <- vapply(param_draws, function(x) x$draw_id, integer(1))

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
      .param_draw_position = seq_along(draw_ids),
      stringsAsFactors = FALSE
    )
    data.frame(
      entity_id = rep(pid, nrow(grid)),
      param_draw_id = draw_ids[grid$.param_draw_position],
      sim_id = grid$sim_id,
      .param_draw_position = grid$.param_draw_position,
      stringsAsFactors = FALSE
    )
  })
  execution_idx <- do.call(rbind, idx_list)
  execution_idx$run_id <- paste0("run_", seq_len(nrow(execution_idx)))
  idx <- execution_idx[c("entity_id", "param_draw_id", "sim_id", "run_id")]

  # Group runs by entity for parallelization (preserve entity_ids order)
  split_by_entity <- stats::setNames(lapply(entity_ids, function(pid) {
    execution_idx[execution_idx$entity_id == pid, , drop = FALSE]
  }),
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
    entity_out <- future.apply::future_lapply(
      names(split_by_entity),
      run_one_entity,
      future.seed = .future_seed_option(seed)
    )
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
    run_id <- rows$run_id[[r]]
    param_draw_id <- rows$param_draw_id[[r]]
    sim_id <- rows$sim_id[[r]]
    draw_position <- rows$.param_draw_position[[r]]
    param_ctx <- param_draws[[draw_position]]

    # Deep clone entity for each run (so sims don't interfere)
    p <- p0$clone(deep = TRUE)

    # Deterministic seeding per (entity, draw, sim) if seed is provided
    if (!is.null(seed)) {
      local_seed <- .seed_for(seed, entity_id, param_draw_id, sim_id)
      set.seed(local_seed)
    }

    run_meta <- list(
      time_spec = canonical_time_spec,
      run_id = run_id,
      entity_id = entity_id,
      param_draw_id = param_draw_id,
      sim_id = sim_id,
      param_ctx = param_ctx,
      .rng_owned_by_harness = TRUE
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
    return(engine$bundle$sample_params(n_param_draws))
  }

  # Fallback: repeat the bundle's default parameter payload (or an empty list)
  # in typed, sequentially identified contexts.
  params <- if (!is.null(engine$bundle$params)) engine$bundle$params else list()
  .default_param_draws(params, n_param_draws)
}

.default_param_draws <- function(params, n_param_draws) {
  lapply(seq_len(n_param_draws), function(draw_id) {
    ParamContext(draw_id = draw_id, params = params)
  })
}

.future_seed_option <- function(seed) {
  # Seeded cohorts allocate their own deterministic coordinate streams inside
  # each worker. NULL disables Future's redundant stream allocation and RNG
  # misuse monitor. For unseeded cohorts, Future becomes the outer RNG owner
  # and assigns parallel-safe streams to entity workers.
  if (is.null(seed)) TRUE else NULL
}

.normalize_param_draws <- function(draws, n_param_draws, source) {
  if (inherits(draws, "ParamContext")) {
    stop(
      sprintf("%s must be an outer list of ParamContext objects, not a single ParamContext.", source),
      call. = FALSE
    )
  }
  if (!is.list(draws) || length(draws) != n_param_draws) {
    stop(
      sprintf("%s must return or supply a list of exactly %d ParamContext object(s).", source, n_param_draws),
      call. = FALSE
    )
  }

  is_context <- vapply(draws, function(x) {
    inherits(x, "ParamContext") && is.list(x)
  }, logical(1))
  if (!all(is_context)) {
    bad <- which(!is_context)
    stop(
      sprintf(
        "%s must contain only ParamContext objects; invalid element position(s): %s. Bare parameter payload lists are not accepted.",
        source,
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  required_fields <- c("draw_id", "params", "provenance")
  valid_shape <- vapply(draws, function(x) {
    field_names <- names(x)
    !is.null(field_names) &&
      !anyNA(field_names) &&
      all(nzchar(field_names)) &&
      all(vapply(required_fields, function(field) sum(field_names == field) == 1L, logical(1))) &&
      is.list(x$params) &&
      (is.null(x$provenance) ||
         (is.character(x$provenance) && length(x$provenance) == 1L))
  }, logical(1))
  if (!all(valid_shape)) {
    stop(
      sprintf(
        "%s contains a malformed ParamContext at element position(s): %s. Each context must carry draw_id, params, and provenance fields with their documented types.",
        source,
        paste(which(!valid_shape), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  valid_id <- vapply(draws, function(x) {
    id <- x$draw_id
    is.integer(id) && length(id) == 1L && !is.na(id) && id > 0L
  }, logical(1))
  if (!all(valid_id)) {
    stop(
      sprintf(
        "%s contains an invalid ParamContext `draw_id` at element position(s): %s. Draw ids must be positive, losslessly integer-valued scalars.",
        source,
        paste(which(!valid_id), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  draw_ids <- vapply(draws, function(x) x$draw_id, integer(1))
  duplicated_ids <- unique(draw_ids[duplicated(draw_ids)])
  if (length(duplicated_ids) > 0L) {
    stop(
      sprintf(
        "%s contains duplicated ParamContext draw id(s): %s.",
        source,
        paste(duplicated_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  draws[order(draw_ids)]
}

.seed_for <- function(base_seed, entity_id, param_draw_id, sim_id) {
  # Deterministic stream seed derived from run coordinates.
  # Formula: base_seed + draw_id * P1 + sim_id * P2 + entity_hash * P3 (mod max_int)
  # P1/P2/P3 are distinct large primes chosen to minimise collisions across
  # typical cohort sizes (<=1e4 entities, <=1e3 draws, <=100 sims).
  # This is an INTERNAL detail. The public contract is:
  #   fixed seed + draw_id + sim_id + entity_id => reproducible output.
  # Stream allocation details must not appear in user-facing documentation.
  # Use double arithmetic deliberately: accepted draw ids can reach the R
  # integer limit, and integer intermediates must not overflow before modulo.
  P1 <- 99991
  P2 <-  9973
  P3 <-   997
  h  <- sum(as.numeric(utf8ToInt(as.character(entity_id)))) %% 100000
  value <- as.numeric(base_seed) +
    P1 * as.numeric(param_draw_id) +
    P2 * as.numeric(sim_id) +
    P3 * h
  as.integer(value %% as.numeric(.Machine$integer.max))
}

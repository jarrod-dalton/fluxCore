# ------------------------------------------------------------------------------
# run_cohort()
#
# Purpose:
#   Run a simulation engine for multiple entities, optionally with repeated
#   parameter draws and repeated simulations per draw.
#
# Time semantics:
#   - The canonical model time spec is declared once in bundle$time_spec.
#   - Runtime ctx may not override time metadata.
#
# Returns:
#   A list containing run index metadata and results (entities, events, observations).
# ------------------------------------------------------------------------------

run_cohort <- function(engine,
                       entities,
                       n_param_draws = 1,
                       n_sims = 1,
                       param_draws = NULL,
                       ctx = NULL,
                       max_events = 1000,
                       max_time = NULL,
                       return_observations = TRUE,
                       backend = NULL,
                       n_workers = NULL,
                       seed = NULL) {

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

  # ctx can be:
  # - NULL
  # - a single list of context fields (merged into each run ctx)
  # - a list of per-draw ctx lists of length n_param_draws
  if (!is.null(ctx)) {
    if (!is.list(ctx)) stop("ctx must be a list, a list of lists, or NULL.", call. = FALSE)
    is_per_draw_ctx <- .ctx_is_per_draw(ctx)
    if (is_per_draw_ctx && length(ctx) != n_param_draws) {
      stop("If ctx is a list of lists, it must have length n_param_draws.", call. = FALSE)
    }
  }
  ctx_user <- ctx

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
      seed = seed,
      ctx_user = ctx_user,
      ctx_is_per_draw = .ctx_is_per_draw(ctx_user)
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
      seed = seed,
      ctx_user = ctx_user,
      ctx_is_per_draw = .ctx_is_per_draw(ctx_user)
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
                                   seed,
                                   ctx_user,
                                   ctx_is_per_draw) {
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

    ctx_run <- list(
      time = .time_ctx_from_spec(canonical_time_spec),
      time_spec = canonical_time_spec,
      entity_id = entity_id,
      param_draw_id = param_draw_id,
      sim_id = sim_id,
      params = param_draws[[param_draw_id]]
    )

    # Merge user-provided ctx (if any). Per-draw ctx overrides single ctx.
    if (!is.null(ctx_user)) {
      base_ctx <- if (ctx_is_per_draw) ctx_user[[param_draw_id]] else ctx_user
      .assert_ctx_time_compatible(
        ctx = base_ctx,
        canonical_time_spec = canonical_time_spec,
        where = sprintf("run_cohort() ctx for entity '%s', draw %d", entity_id, param_draw_id)
      )
      base_ctx$time <- NULL
      base_ctx$time_spec <- NULL

      # Do not allow user ctx to overwrite identifiers; params are allowed.
      ctx_run <- utils::modifyList(ctx_run, base_ctx, keep.null = TRUE)
      ctx_run$entity_id <- entity_id
      ctx_run$param_draw_id <- param_draw_id
      ctx_run$sim_id <- sim_id
      ctx_run$time <- .time_ctx_from_spec(canonical_time_spec)
      ctx_run$time_spec <- canonical_time_spec
      # If base_ctx provides params, honor it; otherwise keep param_draws.
      if (!is.null(base_ctx$params)) ctx_run$params <- base_ctx$params
    }

    out <- engine$run(
      entity = p,
      max_events = max_events,
      max_time = max_time,
      return_observations = return_observations,
      ctx = ctx_run
    )

    out_list[[r]] <- out
  }

  out_list
}

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

.seed_for <- function(base_seed, entity_id, param_draw_id, sim_id) {
  # stable integer seed derived from identifiers
  # (simple; if you want stronger guarantees, switch to L'Ecuyer streams)
  h <- sum(utf8ToInt(as.character(entity_id))) %% 100000L
  as.integer((base_seed + 100000L * param_draw_id + 1000L * sim_id + h) %% .Machine$integer.max)
}

.ctx_is_per_draw <- function(ctx) {
  if (is.null(ctx) || !is.list(ctx) || length(ctx) == 0L) return(FALSE)
  if (!all(vapply(ctx, is.list, logical(1)))) return(FALSE)
  nms <- names(ctx)
  if (!is.null(nms)) {
    reserved <- c("time", "time_spec", "params", "entity_id", "param_draw_id", "sim_id")
    if (any(nzchar(nms) & nms %in% reserved)) return(FALSE)
  }
  TRUE
}

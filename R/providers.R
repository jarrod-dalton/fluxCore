NULL

PackageProvider <- R6::R6Class(
  classname = "PackageProvider",
  public = list(
    registry = NULL,

    initialize = function(registry = NULL) {
      if (is.null(registry)) {
        registry <- list(default = default_model_bundle)
      }
      if (!is.list(registry) || is.null(names(registry)) || any(names(registry) == "")) {
        stop("registry must be a named list of bundle builder functions.")
      }
      for (k in names(registry)) {
        if (!is.function(registry[[k]])) stop(sprintf("registry[['%s']] must be a function.", k))
      }
      self$registry <- registry
      invisible(self)
    },

    load = function(model_spec = list(name = "default"), ...) {
      if (is.null(model_spec) || !is.list(model_spec)) stop("model_spec must be a list.")
      name <- model_spec$name
      if (is.null(name)) name <- "default"
      name <- as.character(name)

      if (!name %in% names(self$registry)) {
        stop(sprintf("Unknown bundle name '%s'. Available: %s", name, paste(names(self$registry), collapse = ", ")))
      }

      builder <- self$registry[[name]]
      bundle <- builder(...)
      .validate_model_bundle(bundle)
      bundle
    },

    sample_param_draws = function(model_spec = list(name = "default"), n_param_draws = 1L, ...) {
      # Optional hook for global parameter draws.
      # Default behavior:
      # - if the bundle builder returns a bundle with $sample_params(D), use it
      # - otherwise return NULL draws
      n_param_draws <- as.integer(n_param_draws)
      if (!is.finite(n_param_draws) || n_param_draws < 1L) stop("n_param_draws must be a positive integer.")

      bundle <- self$load(model_spec = model_spec, ...)
      if (!is.null(bundle$sample_params) && is.function(bundle$sample_params)) {
        draws <- bundle$sample_params(n_param_draws)
        if (!is.list(draws) || length(draws) != n_param_draws) {
          stop("bundle$sample_params(D) must return a list of length D.")
        }
        return(draws)
      }
      rep(list(NULL), n_param_draws)
    }
  )
)

FileProvider <- R6::R6Class(
  classname = "FileProvider",
  public = list(
    base_path = NULL,
    initialize = function(base_path = NULL) {
      self$base_path <- base_path
      invisible(self)
    },
    load = function(model_spec, ...) {
      if (is.null(model_spec) || !is.list(model_spec) || is.null(model_spec$path)) {
        stop("model_spec must be a list with element $path.")
      }
      path <- as.character(model_spec$path)
      if (!is.null(self$base_path)) {
        path <- file.path(self$base_path, path)
      }
      if (!file.exists(path)) stop("File not found: ", path)
      bundle <- readRDS(path)
      .validate_model_bundle(bundle)
      bundle
    },
    sample_param_draws = function(model_spec, n_param_draws = 1L, ...) {
      # If the bundle includes sample_params, use it; else NULL draws.
      n_param_draws <- as.integer(n_param_draws)
      bundle <- self$load(model_spec, ...)
      if (!is.null(bundle$sample_params) && is.function(bundle$sample_params)) {
        draws <- bundle$sample_params(n_param_draws)
        if (!is.list(draws) || length(draws) != n_param_draws) stop("bundle$sample_params(D) must return a list of length D.")
        return(draws)
      }
      rep(list(NULL), n_param_draws)
    }
  )
)

MLflowProvider <- R6::R6Class(
  classname = "MLflowProvider",
  public = list(
    builder_fn = NULL,
    sampler_fn = NULL,

    initialize = function(builder_fn, sampler_fn = NULL) {
      if (is.null(builder_fn) || !is.function(builder_fn)) {
        stop("builder_fn must be a function that returns a ModelBundle.")
      }
      if (!is.null(sampler_fn) && !is.function(sampler_fn)) {
        stop("sampler_fn must be NULL or a function.")
      }
      self$builder_fn <- builder_fn
      self$sampler_fn <- sampler_fn
      invisible(self)
    },

    load = function(model_spec, ...) {
      if (is.null(model_spec) || !is.list(model_spec) || is.null(model_spec$model_uri)) {
        stop("model_spec must be a list with element $model_uri.")
      }
      model_uri <- as.character(model_spec$model_uri)
      artifacts <- model_spec$artifacts
      bundle <- self$builder_fn(model_uri = model_uri, artifacts = artifacts, model_spec = model_spec, ...)
      .validate_model_bundle(bundle)
      bundle
    },

    sample_param_draws = function(model_spec, n_param_draws = 1L, ...) {
      n_param_draws <- as.integer(n_param_draws)
      if (!is.finite(n_param_draws) || n_param_draws < 1L) stop("n_param_draws must be a positive integer.")
      if (is.null(self$sampler_fn)) {
        # fallback to bundle$sample_params if present
        bundle <- self$load(model_spec, ...)
        if (!is.null(bundle$sample_params) && is.function(bundle$sample_params)) {
          draws <- bundle$sample_params(n_param_draws)
          if (!is.list(draws) || length(draws) != n_param_draws) stop("bundle$sample_params(D) must return a list of length D.")
          return(draws)
        }
        return(rep(list(NULL), n_param_draws))
      }
      model_uri <- as.character(model_spec$model_uri)
      artifacts <- model_spec$artifacts
      draws <- self$sampler_fn(model_uri = model_uri, artifacts = artifacts, model_spec = model_spec, D = n_param_draws, ...)
      if (!is.list(draws) || length(draws) != n_param_draws) {
        stop("sampler_fn(...) must return a list of length D.")
      }
      draws
    }
  )
)

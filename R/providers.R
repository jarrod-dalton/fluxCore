NULL

#' PackageProvider (internal)
#'
#' Loads a bundle from in-package functions/objects. Use model_spec$name to select
#' among multiple bundle builders.
#'
#' Deprecated in v2.0.0. Use [load_model()] or `Engine$new(bundle = ...)` instead.
#'
#' @param registry Named list mapping bundle names to functions returning ModelBundles.
#' @keywords internal
PackageProvider <- R6::R6Class(
  classname = "PackageProvider",
  public = list(
    registry = NULL,

    initialize = function(registry) {
      if (missing(registry) || is.null(registry)) {
        stop("registry is required; supply a named list of bundle builder functions.")
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

    load = function(model_spec = list(), ...) {
      if (is.null(model_spec) || !is.list(model_spec)) stop("model_spec must be a list.")
      name <- model_spec$name
      if (is.null(name)) {
        if (length(self$registry) == 1L) {
          name <- names(self$registry)[1]
        } else {
          stop(
            sprintf(
              "model_spec$name is required when registry has multiple bundles. Available: %s",
              paste(names(self$registry), collapse = ", ")
            )
          )
        }
      }
      name <- as.character(name)

      if (!name %in% names(self$registry)) {
        stop(sprintf("Unknown bundle name '%s'. Available: %s", name, paste(names(self$registry), collapse = ", ")))
      }

      builder <- self$registry[[name]]
      bundle <- builder(...)
      .validate_model_bundle(bundle)
      bundle
    },

    sample_param_draws = function(model_spec = list(), n_param_draws = 1L, ...) {
      # Optional hook for global parameter draws.
      # Default behavior:
      # - if the bundle builder returns a bundle with $sample_params(D), use it
      # - otherwise return typed contexts carrying bundle defaults
      n_param_draws <- as.integer(n_param_draws)
      if (!is.finite(n_param_draws) || n_param_draws < 1L) stop("n_param_draws must be a positive integer.")

      bundle <- self$load(model_spec = model_spec, ...)
      if (!is.null(bundle$sample_params) && is.function(bundle$sample_params)) {
        draws <- bundle$sample_params(n_param_draws)
        return(.normalize_param_draws(
          draws,
          n_param_draws,
          "PackageProvider bundle$sample_params(D)"
        ))
      }
      params <- if (!is.null(bundle$params)) bundle$params else list()
      .default_param_draws(params, n_param_draws)
    }
  )
)

#' FileProvider (internal)
#'
#' Loads a bundle from an .rds file. Expects model_spec$path.
#'
#' Deprecated in v2.0.0. Use [load_model()] or `Engine$new(bundle = ...)` instead.
#'
#' @param base_path Optional base directory.
#' @keywords internal
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
      # If the bundle includes sample_params, use it; else typed default draws.
      n_param_draws <- as.integer(n_param_draws)
      if (!is.finite(n_param_draws) || n_param_draws < 1L) stop("n_param_draws must be a positive integer.")
      bundle <- self$load(model_spec, ...)
      if (!is.null(bundle$sample_params) && is.function(bundle$sample_params)) {
        draws <- bundle$sample_params(n_param_draws)
        return(.normalize_param_draws(
          draws,
          n_param_draws,
          "FileProvider bundle$sample_params(D)"
        ))
      }
      params <- if (!is.null(bundle$params)) bundle$params else list()
      .default_param_draws(params, n_param_draws)
    }
  )
)

#' MLflowProvider (internal stub)
#'
#' Scaffold for MLflow-based workflows. model_spec should include at least
#' model_uri and optionally references to artifacts needed to build a bundle.
#'
#' Deprecated in v2.0.0. Use [load_model()] or `Engine$new(bundle = ...)` instead.
#'
#' @param builder_fn Function returning a ModelBundle.
#' @param sampler_fn Optional function returning a list of length D parameter contexts.
#' @keywords internal
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
          return(.normalize_param_draws(
            draws,
            n_param_draws,
            "MLflowProvider bundle$sample_params(D)"
          ))
        }
        params <- if (!is.null(bundle$params)) bundle$params else list()
        return(.default_param_draws(params, n_param_draws))
      }
      model_uri <- as.character(model_spec$model_uri)
      artifacts <- model_spec$artifacts
      draws <- self$sampler_fn(model_uri = model_uri, artifacts = artifacts, model_spec = model_spec, D = n_param_draws, ...)
      .normalize_param_draws(draws, n_param_draws, "MLflowProvider sampler_fn(...)")
    }
  )
)

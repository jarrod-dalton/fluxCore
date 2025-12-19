#' Compose ModelBundles and transition components
#'
#' These helpers make it easy to layer policy/intervention logic on top of baseline
#' natural history dynamics without rewriting the baseline bundle.
#'
#' The key idea is to compose functions that return sparse `changes` patches
#' (named lists). Patch merging is controlled by a simple strategy.
#'
#' @examples
#' library(patientSimCore)
#' base <- default_model_bundle()
#' policy <- list(
#'   transition = function(patient, event_type, time_next, ctx) {
#'     # Example: if age >= 65, set a flag (requires schema includes on_medicare)
#'     if ("on_medicare" %in% names(patient$current) && patient$current$age >= 65) {
#'       return(list(on_medicare = 1))
#'     }
#'     NULL
#'   }
#' )
#' composed <- compose_bundles(base, policy)
#'
#' @export
compose_bundles <- function(baseline, policy = NULL, merge = c("policy_wins", "baseline_wins", "error_on_conflict")) {
  merge <- match.arg(merge)

  if (is.null(policy)) return(baseline)

  # Policy may provide any subset of bundle functions; missing ones fall back to baseline
  out <- baseline

  # Compose propose_event if policy provides it
  if (!is.null(policy$propose_event)) {
    f_base <- baseline$propose_event
    f_pol <- policy$propose_event
    out$propose_event <- function(patient, ctx) {
      # Policy can choose to delegate or override
      f_pol(patient, ctx, baseline_propose = f_base)
    }
  }

  # Compose transition if policy provides it
  if (!is.null(policy$transition)) {
    f_base <- baseline$transition
    f_pol <- policy$transition
    out$transition <- function(patient, event_type, time_next, ctx) {
      ch0 <- f_base(patient, event_type, time_next, ctx)
      ch1 <- f_pol(patient, event_type, time_next, ctx, baseline_changes = ch0)
      merge_patches(ch0, ch1, merge = merge)
    }
  }

  # Compose stop if policy provides it (policy can stop earlier)
  if (!is.null(policy$stop)) {
    f_base <- baseline$stop
    f_pol <- policy$stop
    out$stop <- function(patient, event_type, ctx) {
      isTRUE(f_base(patient, event_type, ctx)) || isTRUE(f_pol(patient, event_type, ctx))
    }
  }

  # Compose observe: stack both (baseline then policy) into one list
  if (!is.null(policy$observe)) {
    f_base <- baseline$observe
    f_pol <- policy$observe
    out$observe <- function(patient, event_type, ctx) {
      o0 <- if (!is.null(f_base)) f_base(patient, event_type, ctx) else NULL
      o1 <- f_pol(patient, event_type, ctx, baseline_obs = o0)
      # merge observations as lists; policy wins on conflicts
      if (is.null(o0)) return(o1)
      if (is.null(o1)) return(o0)
      merge_lists(o0, o1)
    }
  }

  out
}

#' Merge two sparse change patches
#'
#' @param baseline_changes Named list or NULL.
#' @param policy_changes Named list or NULL.
#' @param merge Merge strategy.
#' @return Named list or NULL.
#' @keywords internal
merge_patches <- function(baseline_changes, policy_changes,
                          merge = c("policy_wins", "baseline_wins", "error_on_conflict")) {
  merge <- match.arg(merge)

  if (is.null(baseline_changes) && is.null(policy_changes)) return(NULL)
  if (is.null(baseline_changes)) return(policy_changes)
  if (is.null(policy_changes)) return(baseline_changes)

  if (!is.list(baseline_changes) || !is.list(policy_changes)) {
    stop("Both patches must be lists or NULL.")
  }

  k0 <- names(baseline_changes)
  k1 <- names(policy_changes)
  if (is.null(k0) || is.null(k1)) stop("Patches must be named lists.")

  conflicts <- intersect(k0, k1)
  if (length(conflicts) > 0 && merge == "error_on_conflict") {
    stop(sprintf("Conflicting patch keys: %s", paste(conflicts, collapse = ", ")))
  }

  if (merge == "policy_wins") {
    out <- baseline_changes
    out[k1] <- policy_changes
    return(out)
  }

  if (merge == "baseline_wins") {
    out <- policy_changes
    out[k0] <- baseline_changes
    return(out)
  }

  stop("Unknown merge strategy.")
}

#' Merge two named lists (policy wins)
#' @keywords internal
merge_lists <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.null(y)) return(x)
  out <- x
  out[names(y)] <- y
  out
}

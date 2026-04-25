\name{compose_bundles}
\alias{compose_bundles}
\title{Compose ModelBundles and transition components}
\usage{
compose_bundles(
  baseline,
  policy = NULL,
  merge = c("policy_wins", "baseline_wins", "error_on_conflict")
)
}
\arguments{
\item{baseline}{Baseline model bundle list.}
\item{policy}{Optional policy/intervention bundle list overriding selected components.}
\item{merge}{Patch conflict strategy for transition updates:
\code{"policy_wins"}, \code{"baseline_wins"}, or \code{"error_on_conflict"}.}
}
\description{
These helpers make it easy to layer policy/intervention logic on top of baseline
natural history dynamics without rewriting the baseline bundle.
}
\details{
The key idea is to compose functions that return sparse \code{changes} patches
(named lists). Patch merging is controlled by a simple strategy.
}
\examples{
library(fluxCore)
base <- list(
  time_spec = time_spec(unit = "hours"),
  event_catalog = c("dispatch_check", "end_shift"),
  terminal_events = "end_shift",
  propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    list(dispatch = list(time_next = entity$last_time + 1, event_type = "dispatch_check"))
  },
  transition = function(entity, event, ctx = NULL) list(),
  stop = function(entity, event, ctx = NULL) FALSE
)
policy <- list(
  transition = function(entity, event, ctx = NULL, baseline_changes = NULL) {
    # Example: if battery is low, set recharge mode (requires schema include recharge_mode)
    if ("recharge_mode" \%in\% names(entity$current) &&
        "battery_pct" \%in\% names(entity$current) &&
        is.finite(entity$current$battery_pct) &&
        entity$current$battery_pct < 20) {
      return(list(recharge_mode = "on"))
    }
    NULL
  }
)
composed <- compose_bundles(base, policy)

}

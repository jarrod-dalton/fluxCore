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
base <- default_model_bundle()
policy <- list(
  transition = function(entity, event_type, time_next, ctx) {
    # Example: if age >= 65, set a flag (requires schema includes on_medicare)
    if ("on_medicare" \%in\% names(entity$current) && entity$current$age >= 65) {
      return(list(on_medicare = 1))
    }
    NULL
  }
)
composed <- compose_bundles(base, policy)

}

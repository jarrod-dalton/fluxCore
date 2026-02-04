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
\description{
These helpers make it easy to layer policy/intervention logic on top of baseline
natural history dynamics without rewriting the baseline bundle.
}
\details{
The key idea is to compose functions that return sparse \code{changes} patches
(named lists). Patch merging is controlled by a simple strategy.
}
\examples{
library(patientSimCore)
base <- default_model_bundle()
policy <- list(
  transition = function(patient, event_type, time_next, ctx) {
    # Example: if age >= 65, set a flag (requires schema includes on_medicare)
    if ("on_medicare" \%in\% names(patient$current) && patient$current$age >= 65) {
      return(list(on_medicare = 1))
    }
    NULL
  }
)
composed <- compose_bundles(base, policy)

}

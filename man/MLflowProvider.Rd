\name{MLflowProvider}
\alias{MLflowProvider}
\title{MLflowProvider (stub)}
\arguments{
\item{builder_fn}{Function returning a ModelBundle.}

\item{sampler_fn}{Optional function returning a list of length D parameter contexts.}
}
\description{
Scaffold for MLflow-based workflows. \code{model_spec} should include at least
\code{model_uri} and optionally references to artifacts needed to build a bundle.
}
\details{
This provider delegates to \code{builder_fn(model_uri, artifacts, model_spec, ...)}.

You can also supply an optional \code{sampler_fn(model_uri, artifacts, model_spec, D, ...)}
to generate global parameter draws for draw-aware components (e.g., regression models
with \code{cov(beta)}).
}
\examples{
library(fluxCore)
prov <- MLflowProvider$new(
  builder_fn = function(model_uri, artifacts = NULL, model_spec = NULL) {
    list(
      time_spec = time_spec(unit = "hours"),
      event_catalog = c("dispatch_check", "end_shift"),
      terminal_events = "end_shift",
      propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
        list(dispatch = list(time_next = entity$last_time + 1, event_type = "dispatch_check"))
      },
      transition = function(entity, event, ctx = NULL) list(),
      stop = function(entity, event, ctx = NULL) FALSE
    )
  }
)
bundle <- prov$load(list(model_uri = "models:/my_model@champion"))

}

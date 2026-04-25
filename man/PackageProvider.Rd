\name{PackageProvider}
\alias{PackageProvider}
\title{PackageProvider}
\arguments{
\item{registry}{Named list mapping bundle names to functions returning ModelBundles.}
}
\description{
Loads a bundle from in-package functions/objects. Use \code{model_spec$name} to select
among multiple bundle builders.
}
\examples{
library(fluxCore)
prov <- PackageProvider$new(
  registry = list(
    delivery = function() {
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
)
bundle <- prov$load(list(name = "delivery"))
names(bundle)

}

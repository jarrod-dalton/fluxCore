\name{FileProvider}
\alias{FileProvider}
\title{FileProvider}
\arguments{
\item{base_path}{Optional base directory.}
}
\description{
Loads a bundle from an \code{.rds} file. Expects \code{model_spec$path}.
}
\details{
Note: serializing functions can be brittle across environments. Often better:
store parameters/reference data on disk and rebuild the bundle at load time.
}
\examples{
\dontrun{
library(fluxCore)
bundle <- list(
  time_spec = time_spec(unit = "hours"),
  event_catalog = c("dispatch_check", "end_shift"),
  terminal_events = "end_shift",
  propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    list(dispatch = list(time_next = entity$last_time + 1, event_type = "dispatch_check"))
  },
  transition = function(entity, event, ctx = NULL) list(),
  stop = function(entity, event, ctx = NULL) FALSE
)
saveRDS(bundle, "bundle.rds")
prov <- FileProvider$new()
bundle <- prov$load(list(path = "bundle.rds"))
}

}

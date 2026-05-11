\name{modelbundle_concept}
\alias{modelbundle_concept}
\title{ModelBundle contract (concept)}
\description{
A ModelBundle is a named list of callables that define simulation dynamics.
The Engine uses these functions to propose the next event, compute state updates,
and decide when to stop.
}
\details{
Required functions:
\itemize{
\item \code{propose_events(entity, sim_ctx, param_ctx, ...)} -> named list of event proposals keyed by \code{process_id}
\item \code{transition(entity, event, sim_ctx, param_ctx)} -> named list \code{changes} or NULL
\item \code{stop(entity, event, sim_ctx)} -> TRUE/FALSE
}

Optional functions:
\itemize{
\item \code{observe(entity, event, sim_ctx, param_ctx)} -> list/data.frame row of observed outputs for logging
\item \code{refresh_rules(entity, last_event, changes, sim_ctx)} -> exactly \code{"ALL"} or character vector of unique \code{process_id}s
}

A bundle can close over fitted models, parameters, reference data, etc. This makes
it easy to swap dynamics while keeping the Entity and Engine generic.

All proposed event times (\code{time_next}) live on one shared numeric time axis. The
package does not enforce units; bundles should document the intended unit (typically via
\code{sim_ctx$time_spec$unit}).

For compatibility with v1.x bundles, callbacks that still use \code{ctx} are currently
accepted with advisory warnings during v2 migration.

If \code{refresh_rules} is omitted, Engine defaults to \code{"ALL"} refresh behavior after each event.
}
\keyword{internal}

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
\item \code{propose_events(patient, ctx, ...)} -> named list of event proposals keyed by \code{process_id}
\item \code{transition(patient, event, ctx)} -> named list \code{changes} or NULL
\item \code{stop(patient, event, ctx)} -> TRUE/FALSE
}

Optional functions:
\itemize{
\item \code{observe(patient, event, ctx)} -> list/data.frame row of observed outputs for logging
\item \code{refresh_rules(patient, last_event, changes, ctx)} -> "ALL" or character vector of \code{process_id}s
}

A bundle can close over fitted models, parameters, reference data, etc. This makes
it easy to swap dynamics while keeping the Patient and Engine generic.

All proposed event times (\code{time_next}) live on one shared numeric time axis. The
package does not enforce units; bundles should document the intended unit (typically via
\code{ctx$time$unit}).
}
\keyword{internal}

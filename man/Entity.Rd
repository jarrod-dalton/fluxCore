\name{Entity}
\alias{Entity}
\title{Entity}
\description{
R6 simulation state container used by \code{fluxCore::Engine}. An \code{Entity}
stores current state, event history, derived-variable registrations, and metadata
such as \code{id} and optional \code{meta$entity_type}.
}
\details{
\code{Entity} is created via \code{Entity$new(...)}, which applies schema
validation/coercion and initializes the baseline event at \code{time0}.

Common public methods include:
\itemize{
\item \code{$state()} and \code{$as_list()} for current state access
\item \code{$update(time, event_type, changes)} to apply an event transition
\item \code{$state_at()} / \code{$state_at_time()} for historical state lookup
\item \code{$snapshot()} / \code{$snapshot_at()} / \code{$snapshot_at_time()} for
state plus derived variables
}
}

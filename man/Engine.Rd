\name{Engine}
\alias{Engine}
\title{Engine}
\description{
Orchestrates simulation by repeatedly proposing the next event(s), applying a transition
patch, recording the event on a \code{Entity}, and stopping when \code{bundle$stop()} returns TRUE
(or a \code{max_time} / \code{max_events} limit is reached).
}
\details{
The Engine requires \code{bundle$propose_events(entity, ...)} returning one proposed future event
per process, as a named list keyed by \code{process_id}.

When multiple processes are present, the Engine caches each process's next proposal and
refreshes proposals on-demand using \code{bundle$refresh_rules()}.
If omitted, refresh behavior defaults to \code{"ALL"} (refresh all process clocks).
When implemented, \code{refresh_rules()} must return exactly \code{"ALL"} or a
character vector of unique \code{process_id} values.

Time is represented as a numeric value on a single global time axis. The Engine does not
enforce units at the arithmetic level; for clarity and consistency (rates, cadences,
lookbacks), models must declare a canonical \code{bundle$time_spec} via
\code{time_spec(unit = ...)}.
}
\section{Fields}{

\itemize{
\item \code{provider}: a ModelProvider (e.g., \link{PackageProvider}, \link{FileProvider}).
  May be \code{NULL} when the engine was constructed via the \code{bundle} shortcut.
\item \code{model_spec}: named list describing what to load. \code{NULL} when constructed
  via the \code{bundle} shortcut.
\item \code{bundle}: the loaded ModelBundle (named list of functions).
}
}

\section{Constructor}{
\code{Engine$new(bundle = NULL, provider = NULL, model_spec = list(name = "default"), ...)}

Two ways to wire a model:

\itemize{
\item \strong{Direct bundle (recommended for in-memory / inline models).} Pass
  \code{bundle = my_bundle}. The engine validates the bundle and uses it as-is;
  \code{provider} and \code{model_spec} are ignored and stored as \code{NULL}.
\item \strong{Provider (for packaged or pluggable models).} Pass a
  \code{ModelProvider} (e.g. \code{PackageProvider$new(registry = ...)}); the engine
  calls \code{provider$load(model_spec, ...)} to obtain the bundle.
}

Supplying both \code{bundle} and \code{provider} is an error.
}

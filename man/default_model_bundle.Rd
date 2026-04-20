\name{default_model_bundle}
\alias{default_model_bundle}
\title{Default toy ModelBundle}
\usage{
default_model_bundle(terminal_event_type = "death")
}
\arguments{
\item{terminal_event_type}{Character scalar. Default \code{"death"}.}
}
\value{
A ModelBundle (named list of functions).
}
\description{
Provides a small, self-contained bundle that demonstrates:
\itemize{
\item non-uniform inter-event times
\item event types including a terminal \code{"death"}
\item sparse state updates via \code{changes} patches
\item multi-process proposal via \code{propose_events()} (single default process)
}
}
\details{
The toy model assumes your schema contains \code{age} and optionally \code{miles_to_work}.
}
\examples{
library(fluxCore)
set.seed(1)
bundle <- default_model_bundle()
names(bundle)

}

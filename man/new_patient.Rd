\name{new_patient}
\alias{new_patient}
\title{Construct a new Patient}
\usage{
new_patient(
  init,
  schema = default_patient_schema(),
  time0 = 0,
  event_type0 = "init"
)
}
\arguments{
\item{init}{Named list of initial state values. Names must be in \code{schema}.}

\item{schema}{State schema (named list). See \code{default_patient_schema()}.}

\item{time0}{Initial event time (numeric scalar, default 0).}

\item{event_type0}{Initial event type (character, default "init").}
}
\value{
A \code{Patient} R6 object.
}
\description{
Convenience constructor wrapping \code{Patient$new()}.
}
\examples{
library(patientSimCore)
p <- new_patient(init = list(age = 50, miles_to_work = 8))
p$state(c("age", "miles_to_work"))

}

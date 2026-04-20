\name{new_entity}
\alias{new_entity}
\title{Construct a new Entity}
\usage{
new_entity(
  init = list(),
  schema = default_entity_schema(),
  entity_type = NULL,
  time0 = 0,
  event_type0 = "init"
)
}
\arguments{
\item{init}{Named list of initial state values. Names must be in \code{schema}.}

\item{schema}{State schema (named list). See \code{default_entity_schema()}.}

\item{entity_type}{Optional entity type tag stored in \code{meta$entity_type}.}

\item{time0}{Initial event time (numeric scalar, default 0).}

\item{event_type0}{Initial event type (character, default "init").}
}
\value{
A \code{Entity} R6 object.
}
\description{
Convenience constructor wrapping \code{Entity$new()}.
}
\examples{
library(fluxCore)
p <- new_entity(init = list(alive = TRUE))
p$state("alive")

p2 <- new_entity(
  init = list(alive = TRUE),
  entity_type = "entity"
)
p2$meta$entity_type

}

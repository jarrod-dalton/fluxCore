\name{combine_updates}
\alias{combine_updates}
\title{Combine multiple update lists into one atomic update payload}
\description{
Convenience helper for \code{transition()} implementations that need to apply
multiple update sources (e.g., scalar tweaks plus one or more block updates).
The function concatenates named lists and errors if any variable is updated more
than once within the same event.
}
\usage{
combine_updates(...)
}
\arguments{
\item{...}{One or more named lists of updates (or NULL).}
}
\value{
A single named list of updates, or NULL if all inputs are NULL.
}
\examples{
combine_updates(
  list(age = 41),
  list(sbp = 125, dbp = 87)
)
}

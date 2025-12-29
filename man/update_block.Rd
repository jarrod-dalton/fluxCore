\name{update_block}
\alias{update_block}
\title{Block-oriented state updates for vectorized models}
\description{
Many models generate multivariate outputs (e.g., SBP/DBP or lab panels).\cr
\code{update_block()} validates a named payload against a patient's schema and a
named schema block, then returns a named list of per-variable state updates for
use as a \code{transition()} return value.
}
\usage{
update_block(patient, block, values, require_all = TRUE,
  unknown = c("error", "drop_warn_once", "drop_warn_always"))
}
\arguments{
\item{patient}{A \code{Patient} object.}
\item{block}{Block name (character scalar) corresponding to \code{schema$<var>$blocks}.}
\item{values}{Named vector or named list of values to update. Names must correspond
to state variables in the patient's schema.}
\item{require_all}{Logical; if TRUE (default) all variables belonging to \code{block}
must be supplied in \code{values}.}
\item{unknown}{Policy for variables supplied in \code{values} that are not present in
the patient's schema. One of \code{"error"} (default), \code{"drop_warn_once"}, or
\code{"drop_warn_always"}.}
}
\value{
A named list of state updates suitable for returning from \code{transition()}. Values
are returned in schema block order.
}
\examples{
schema <- default_patient_schema()
schema$sbp <- list(default=120, coerce=as.numeric, validate=NULL, blocks=c("bp"))
schema$dbp <- list(default=80, coerce=as.numeric, validate=NULL, blocks=c("bp"))
p <- new_patient(schema = schema)
update_block(p, "bp", c(sbp=125, dbp=87))
}

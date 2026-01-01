\name{default_patient_schema}
\alias{default_patient_schema}
\title{Default patient state schema}
\usage{
default_patient_schema()
}
\value{
A named list (schema) suitable for \code{Patient$new(schema = ...)} and \code{new_patient(schema = ...)}.
}
\description{
A state schema defines the set of core state attributes, their defaults, coercion and validation.
Each entry in the schema is a \emph{variable descriptor} list.
}
\details{
A schema is a named list: \code{var_name -> var_descriptor}.

Each \code{var_descriptor} is a list with fields:

\itemize{
\item \code{default} (required): scalar default value used when not supplied in \code{init}.
\item \code{coerce} (optional): function to coerce values (e.g., \code{as.numeric}).
\item \code{validate} (optional): predicate \code{function(x) TRUE/FALSE} applied after coercion.
\item \code{required} (optional): if \code{TRUE}, the variable must be supplied in \code{init} when creating a patient.
\item \code{blocks} (optional): character vector of schema block names used by \code{update_block()} (many-to-many).
\item \code{type} (optional): variable type hint for downstream summaries. One of \code{binary}, \code{categorical}, \code{continuous}, \code{count}.
\item \code{levels} (optional): for \code{type='categorical'}, the allowed level labels.
}

See \code{inst/SCHEMA_SPEC.md} for the full schema specification and additional examples.
}
\examples{
library(patientSimCore)
schema <- default_patient_schema()
names(schema)
schema$age$default
}

\name{schema_validate}
\alias{schema_validate}
\title{Validate a patient schema}
\description{
Validates the structure and required metadata of a patient schema.
The schema is a named list where each entry describes one state variable.
}
\usage{
schema_validate(schema)
}
\arguments{
\item{schema}{A named list schema.}
}
\value{
The validated schema (invisibly), with normalized type fields.
}
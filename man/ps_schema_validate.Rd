\name{ps_schema_validate}
\alias{ps_schema_validate}
\title{Validate a patient schema}
\description{
Validates the structure and required metadata of a patient schema.
The schema is a named list where each entry describes one state variable.
}
\usage{
ps_schema_validate(schema)
}
\arguments{
\item{schema}{A named list schema.}
}
\value{
The validated schema (invisibly), with normalized type fields.
}
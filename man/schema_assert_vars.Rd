\name{schema_assert_vars}
\alias{schema_assert_vars}
\title{Assert that variables exist in a schema}
\description{Stops with an informative error if any requested variable names are not present in the schema.}
\usage{
schema_assert_vars(schema, vars)
}
\arguments{
\item{schema}{A validated schema (or a raw schema that can be validated).}
\item{vars}{Character vector of variable names.}
}
\value{Invisibly returns TRUE.}

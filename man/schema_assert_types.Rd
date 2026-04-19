\name{schema_assert_types}
\alias{schema_assert_types}
\title{Assert that variables have allowed schema types}
\description{Stops with an informative error if any variables have schema types outside the allowed set.}
\usage{
schema_assert_types(schema, vars, allowed_types)
}
\arguments{
\item{schema}{A validated schema (or a raw schema that can be validated).}
\item{vars}{Character vector of variable names.}
\item{allowed_types}{Character vector of allowed schema types.}
}
\value{Invisibly returns TRUE.}
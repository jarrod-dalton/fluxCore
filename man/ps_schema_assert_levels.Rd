\name{ps_schema_assert_levels}
\alias{ps_schema_assert_levels}
\title{Assert that levels are declared in the schema}
\description{For binary/categorical/ordinal variables, stops if any provided levels are not declared in the schema.}
\usage{
ps_schema_assert_levels(schema, var, levels)
}
\arguments{
\item{schema}{A validated schema (or a raw schema that can be validated).}
\item{var}{A single variable name.}
\item{levels}{Character vector of levels to check.}
}
\value{Invisibly returns TRUE.}
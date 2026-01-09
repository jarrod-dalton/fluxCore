\name{ps_schema_var_info}
\alias{ps_schema_var_info}
\title{Get schema metadata for variables}
\description{Returns schema metadata (type, levels, blocks) for the requested variables.}
\usage{
ps_schema_var_info(schema, vars)
}
\arguments{
\item{schema}{A validated schema (or a raw schema that can be validated).}
\item{vars}{Character vector of variable names.}
}
\value{A data.frame with columns \code{var}, \code{type}, \code{levels}, and \code{blocks}.}
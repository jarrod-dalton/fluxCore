\name{check_derived}
\alias{check_derived}
\title{Register derived variables on an Entity}
\usage{check_derived(entity, derived_vars, replace = FALSE)}
\arguments{
\item{entity}{A \code{Entity} object.}
\item{derived_vars}{Named list of derived-variable functions, or \code{NULL}.}
\item{replace}{If \code{TRUE}, overwrite existing derived vars with the same names.}
}
\value{Returns \code{entity} invisibly.}
\description{Validates and registers derived-variable functions for snapshot-time evaluation.}

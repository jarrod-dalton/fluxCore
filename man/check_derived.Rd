\name{check_derived}
\alias{check_derived}
\title{Register derived variables on a Patient}
\usage{check_derived(patient, derived_vars, replace = FALSE)}
\arguments{
\item{patient}{A \code{Patient} object.}
\item{derived_vars}{Named list of derived-variable functions, or \code{NULL}.}
\item{replace}{If \code{TRUE}, overwrite existing derived vars with the same names.}
}
\value{Returns \code{patient} invisibly.}
\description{Validates and registers derived-variable functions for snapshot-time evaluation.}

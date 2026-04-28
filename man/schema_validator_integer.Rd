\name{schema_validator_integer}
\alias{schema_validator_integer}
\title{Create an integer schema validator}
\description{Returns a predicate function that checks a scalar integer value against inclusive numeric bounds and optional missingness.}
\usage{
schema_validator_integer(min = -Inf, max = Inf, allow_na = FALSE)
}
\arguments{
\item{min}{A single numeric lower bound.}
\item{max}{A single numeric upper bound.}
\item{allow_na}{Logical scalar indicating whether missing values are permitted.}
}
\value{A function of signature \code{function(x) -> TRUE/FALSE}.}

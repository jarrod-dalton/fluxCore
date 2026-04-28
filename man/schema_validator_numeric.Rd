\name{schema_validator_numeric}
\alias{schema_validator_numeric}
\title{Create a numeric schema validator}
\description{Returns a predicate function that checks a scalar numeric value against inclusive numeric bounds and optional missingness.}
\usage{
schema_validator_numeric(min = -Inf, max = Inf, allow_na = FALSE)
}
\arguments{
\item{min}{A single numeric lower bound.}
\item{max}{A single numeric upper bound.}
\item{allow_na}{Logical scalar indicating whether missing values are permitted.}
}
\value{A function of signature \code{function(x) -> TRUE/FALSE}.}

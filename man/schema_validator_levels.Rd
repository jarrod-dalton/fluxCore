\name{schema_validator_levels}
\alias{schema_validator_levels}
\title{Create a levels-based schema validator}
\description{Returns a predicate function that checks a scalar value against a set of allowed character levels and optional missingness.}
\usage{
schema_validator_levels(levels, allow_na = FALSE)
}
\arguments{
\item{levels}{A character vector of allowed level labels.}
\item{allow_na}{Logical scalar indicating whether missing values are permitted.}
}
\value{A function of signature \code{function(x) -> TRUE/FALSE}.}

\name{time_to_model}
\alias{time_to_model}
\title{Convert calendar time to numeric model time}
\description{Converts numeric, Date, or POSIXct/POSIXt time to numeric model time under a compiled time spec.}
\usage{time_to_model(x, time_spec)}
\arguments{
\item{x}{A numeric vector, \code{Date}, or \code{POSIXct}/\code{POSIXt} vector of times.}
\item{time_spec}{A compiled time spec from \code{time_spec(ctx)}.}
}
\value{Numeric model time in units of \code{time_spec$unit}.}
\details{Use \code{time_spec(ctx)} once per workflow, then call \code{time_to_model()} repeatedly for fast conversion.}
\examples{ctx <- set_time_unit(unit = 'days')
spec <- time_spec(ctx)
time_to_model(as.Date('1970-01-02'), spec)
}

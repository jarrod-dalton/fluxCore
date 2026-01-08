\name{ps_time_from_model}
\alias{ps_time_from_model}
\title{Convert numeric model time to calendar time}
\description{Converts numeric model time to Date or POSIXct using a compiled time spec.}
\usage{ps_time_from_model(t, time_spec, class = c('origin', 'Date', 'POSIXct'))}
\arguments{
\item{t}{Numeric model time.}
\item{time_spec}{A compiled time spec from \code{ps_time_spec(ctx)}.}
\item{class}{Output class: \code{'origin'} (match the origin class), \code{'Date'}, or \code{'POSIXct'}.}
}
\value{A \code{Date} or \code{POSIXct} vector.}
\details{The conversion uses fixed unit constants for \code{'months'} and \code{'years'} (30.4375 and 365.25 days).}
\examples{ctx <- ps_set_time_unit(unit = 'hours')
spec <- ps_time_spec(ctx)
ps_time_from_model(2, spec, class = 'POSIXct')
}

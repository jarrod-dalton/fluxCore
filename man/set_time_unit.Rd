\name{set_time_unit}
\alias{set_time_unit}
\title{Set time settings in ctx}
\description{Convenience helper to set \code{ctx$time$unit} and optionally \code{ctx$time$origin} and \code{ctx$time$zone}.}
\usage{set_time_unit(ctx = NULL, unit, origin = NULL, zone = 'UTC')}
\arguments{
\item{ctx}{Context list (or \code{NULL} to create a new one).}
\item{unit}{Time unit string. Must be one of: seconds, minutes, hours, days, weeks, months, years.}
\item{origin}{Optional \code{Date} or \code{POSIXct} origin used as a mapping reference. Default is system epoch.}
\item{zone}{Optional IANA/Olson time zone string. Default is \code{'UTC'}.}
}
\value{Updated ctx list (validated).}
\details{This helper validates the resulting \code{ctx$time} settings by compiling a time spec internally.}
\examples{ctx <- set_time_unit(unit = 'weeks')
}

\name{time_spec}
\alias{time_spec}
\title{Compile and validate canonical time settings}
\description{Compiles and validates calendar-to-model time settings from explicit arguments (preferred) or \code{ctx$time} (compatibility).}
\usage{time_spec(unit = NULL, origin = NULL, zone = "UTC", ctx = NULL)}
\arguments{
\item{unit}{Required time unit when using explicit arguments. One of \code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"}, \code{"weeks"}, \code{"months"}, \code{"years"}.}

\item{origin}{Optional \code{Date} or \code{POSIXct}/\code{POSIXt} origin used for calendar-time conversion. Defaults to Unix epoch.}

\item{zone}{Time zone used for calendar-time conversion (default \code{"UTC"}).}

\item{ctx}{Optional compatibility path. A context list containing \code{ctx$time$unit} and optional \code{ctx$time$origin}/\code{ctx$time$zone}.}
}
\value{An object of class \code{time_spec} with precomputed conversion constants.}
\details{The ecosystem operates on a numeric model time axis. \code{time_spec()} compiles time settings once so repeated conversions can be performed efficiently. The \code{origin} is a system mapping reference, not the model baseline. Units \code{'months'} and \code{'years'} are fixed approximations (30.4375 and 365.25 days).}
\examples{spec <- time_spec(unit = 'weeks')
}

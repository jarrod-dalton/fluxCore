\name{time_spec}
\alias{time_spec}
\title{Compile and validate time settings from ctx}
\description{Compiles and validates calendar-to-model time settings from \code{ctx$time}.}
\usage{time_spec(ctx)}
\arguments{
\item{ctx}{Context list. Must contain \code{ctx$time}, a list with \code{unit} (required), and optional \code{origin} and \code{zone}.}
}
\value{An object of class \code{time_spec} with precomputed conversion constants.}
\details{The ecosystem operates on a numeric model time axis. \code{time_spec()} compiles time settings once so repeated conversions can be performed efficiently. The \code{origin} is a system mapping reference, not the model baseline. Units \code{'months'} and \code{'years'} are fixed approximations (30.4375 and 365.25 days).}
\examples{ctx <- set_time_unit(unit = 'weeks')
spec <- time_spec(ctx)
}

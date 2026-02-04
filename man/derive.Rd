\name{derive}
\alias{derive}
\title{Define a derived feature}
\usage{
derive(
  name,
  target,
  lookback_t = NULL,
  lookback_j = NULL,
  fn = c("count", "min", "max", "mean", "median", "sd", "iqr", "any", "all"),
  include_current = TRUE,
  force = FALSE,
  na_value = NA,
  clock = "time"
)
}
\arguments{
\item{name}{Name of the derived feature.}

\item{target}{A target created by \code{\link[=event]{event()}} or \code{\link[=declare_variable]{declare_variable()}}.}

\item{lookback_t}{Optional numeric lookback window on the time axis. Takes precedence over \code{lookback_j}.}

\item{lookback_j}{Optional integer lookback window in event-index units.}

\item{fn}{Summary function name. For \code{event()} targets, only \code{count}, \code{any}, and \code{all} are supported.}

\item{include_current}{Logical; include the boundary event at \code{t}/\code{j} (default TRUE).}

\item{force}{Logical; if TRUE, return a value when there is no history (default FALSE).}

\item{na_value}{Value to return when \code{force=TRUE} and no history exists (default \code{NA}).}

\item{clock}{Which column in \code{patient$events} defines the time axis for \code{lookback_t} (default \code{"time"}).}
}
\value{
A function \verb{(patient, j, t) -> scalar or NULL}.
}
\description{
\code{derive()} returns a function computing a history-derived feature as of \verb{(j, t)}.
Intended to populate \code{Patient$derived_vars} and be used by \verb{Patient$snapshot*()}.
}

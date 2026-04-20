\name{lag_of}
\alias{lag_of}
\title{Define a lagged feature from variable history}
\usage{
lag_of(
  name,
  target,
  k = 1,
  lookback_t = NULL,
  lookback_j = NULL,
  include_current = FALSE,
  force = FALSE,
  na_value = NA,
  clock = "time"
)
}
\arguments{
\item{name}{Name of the derived feature.}

\item{target}{A target created by \code{\link[=declare_variable]{declare_variable()}}.}

\item{k}{Positive integer lag order (1 = most recent prior value by default).}

\item{lookback_t}{Optional numeric lookback window on the time axis. Takes precedence over \code{lookback_j}.}

\item{lookback_j}{Optional integer lookback window in event-index units.}

\item{include_current}{Logical; include boundary event at \code{t}/\code{j} (default FALSE).}

\item{force}{Logical; if TRUE return \code{na_value} when insufficient history exists (default FALSE).}

\item{na_value}{Value to return when \code{force=TRUE} and insufficient history exists (default \code{NA}).}

\item{clock}{Which column in \code{entity$events} defines the time axis for \code{lookback_t} (default \code{"time"}).}
}
\value{
A function \verb{(entity, j, t) -> scalar or NULL}.
}
\description{
Extract the k-th most recent value of a variable from sparse history as of \verb{(j, t)}.
}

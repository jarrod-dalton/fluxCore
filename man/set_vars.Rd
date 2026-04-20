\name{set_vars}
\alias{set_vars}
\title{Create a named list of updates from variable names and values}
\usage{
set_vars(vars, values)
}
\arguments{
\item{vars}{Character vector of variable names.}

\item{values}{Vector of values (same length as \code{vars}).}
}
\value{
Named list suitable for \code{transition()} returns.
}
\description{
Many models generate multivariate predictions (e.g., SBP/DBP, CBC panels) but
\code{Entity$update()} expects a named list of scalar changes. \code{set_vars()}
converts vectors into the expected named-list format.
}

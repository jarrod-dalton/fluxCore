\name{declare_variable}
\alias{declare_variable}
\title{Target a variable for history-derived features}
\usage{
declare_variable(name)
}
\arguments{
\item{name}{Single variable name (character scalar).}
}
\value{
A target list used by \code{\link[=derive]{derive()}} and \code{\link[=lag_of]{lag_of()}}.
}
\description{
\code{declare_variable()} constructs a target that refers to a state variable's
sparse history in \code{entity$hist}. It is used to define derived features
based on recent values (e.g., counts, means, lags) when building model-ready
snapshots.
}
\examples{
# Create a target referring to LDL history
ldl_target <- declare_variable("ldl")

# Define a derived feature: mean LDL over the past year
mean_ldl_1y <- derive(
  name = "mean_ldl_1y",
  target = ldl_target,
  lookback_t = 1,
  fn = "mean",
  force = TRUE
)
}

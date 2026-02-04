\name{block_vars}
\alias{block_vars}
\title{Get variable names in a schema block}
\usage{
block_vars(schema, block)
}
\arguments{
\item{schema}{A patient state schema (named list).}

\item{block}{Block name (character scalar).}
}
\value{
Character vector of variable names in the order they appear in \code{schema}.
}
\description{
Convenience helper to look up the variables belonging to a named schema block
(e.g., \code{"bp"}, \code{"cbc"}, \code{"cbc_diff"}, \code{"bmp"}, \code{"cmp"}).
Membership is many-to-many: a variable may appear in multiple blocks.
}

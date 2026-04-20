\name{schema_blocks}
\alias{schema_blocks}
\title{List unique schema blocks}
\usage{
schema_blocks(schema)
}
\arguments{
\item{schema}{An entity state schema (named list).}
}
\value{
Character vector of unique block names.
}
\description{
Extract the set of unique block names declared in a schema via the optional
\code{blocks} metadata field on each variable entry. Variables may belong to
multiple blocks (many-to-many).
}

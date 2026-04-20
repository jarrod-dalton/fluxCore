\name{PackageProvider}
\alias{PackageProvider}
\title{PackageProvider}
\arguments{
\item{registry}{Named list mapping bundle names to functions returning ModelBundles.}
}
\description{
Loads a bundle from in-package functions/objects. Use \code{model_spec$name} to select
among multiple bundle builders. By default, \code{"default"} maps to \code{\link[=default_model_bundle]{default_model_bundle()}}.
}
\examples{
library(fluxCore)
prov <- PackageProvider$new()
bundle <- prov$load(list(name = "default"))
names(bundle)

}

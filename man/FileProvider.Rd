\name{FileProvider}
\alias{FileProvider}
\title{FileProvider}
\arguments{
\item{base_path}{Optional base directory.}
}
\description{
Loads a bundle from an \code{.rds} file. Expects \code{model_spec$path}.
}
\details{
Note: serializing functions can be brittle across environments. Often better:
store parameters/reference data on disk and rebuild the bundle at load time.
}
\examples{
\dontrun{
library(patientSimCore)
saveRDS(default_model_bundle(), "bundle.rds")
prov <- FileProvider$new()
bundle <- prov$load(list(path = "bundle.rds"))
}

}

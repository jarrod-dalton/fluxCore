\name{run_cohort}
\alias{run_cohort}
\title{Run a cohort of entities (serial or parallel) with optional global parameter draws}
\usage{
run_cohort(
  engine,
  entities,
  n_param_draws = 1,
  n_sims = 1,
  param_draws = NULL,
  ctx = NULL,
  max_events = 1000,
  max_time = NULL,
  return_observations = TRUE,
  backend = NULL,
  n_workers = NULL,
  seed = NULL
)
}
\arguments{
\item{engine}{An \code{Engine} object (with a materialized \code{bundle}).}

\item{entities}{List of \code{Entity} objects.}

\item{n_param_draws}{Integer; number of global parameter draws (D). Default 1.}

\item{n_sims}{Integer; number of stochastic sims per entity per draw (S). Default 1.}

\item{param_draws}{Optional; a list of length D with per-draw parameter contexts. If NULL,
the function will attempt to call \code{engine$bundle$sample_params(D)} or \code{engine$provider$sample_param_draws(...)}
when available; otherwise it uses a single NULL draw.}

\item{ctx}{Optional context. Either a single list merged into each run context, or a
list of per-draw context lists of length \code{n_param_draws}.}

\item{max_events}{Max events per run.}

\item{max_time}{Optional max time per run.}

\item{return_observations}{Logical; whether to return observations (if bundle provides observe()).}

\item{backend}{Backend used to parallelize across entities. One of \code{"none"}, \code{"cluster"}, \code{"mclapply"}, or \code{"future"}. Default is \code{"none"}. \code{"cluster"} uses a PSOCK cluster (cross-platform). \code{"mclapply"} uses forking (macOS/Linux only). \code{"future"} uses \code{future.apply::future_lapply()} and respects the user's \code{future} plan.}

\item{n_workers}{Integer; workers for parallel; default \code{parallel::detectCores() - 1}.}

\item{seed}{Optional base seed for reproducibility.}
}
\value{
A list with:
\itemize{
\item \code{runs}: list of per-run outputs (entity/events/observations) with labels
\item \code{index}: data.frame mapping run_id -> entity_id/param_draw_id/sim_id
}
}
\description{
These helpers support batch simulation with:
\itemize{
\item global parameter draws reused across entities (parameter uncertainty)
\item multiple stochastic sims per entity per draw (stochastic uncertainty)
\item parallelization across entities
}
}
\details{
The Engine remains entity-oriented; batch utilities orchestrate repetition and labeling.
\subsection{Global parameter draws}{

The recommended pattern is:
\enumerate{
\item Sample \code{D} global parameter draws once.
\item For each entity, and for each draw, run \code{S} stochastic sims.
}

Not all component models must be draw-aware. Components that do not support parameter
uncertainty can ignore \code{ctx$params} (or simply have no entry in \code{ctx$params}).
}
}
\examples{
library(fluxCore)
set.seed(1)

schema <- list(
  route_zone = list(
    type = "categorical",
    levels = c("urban", "suburban", "rural"),
    default = "urban",
    coerce = as.character
  ),
  battery_pct = list(type = "continuous", default = 100, coerce = as.numeric)
)

toy_bundle <- list(
  time_spec = time_spec(unit = "hours"),
  propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    list(dispatch = list(time_next = entity$last_time + 1, event_type = "dispatch_check"))
  },
  transition = function(entity, event, ctx = NULL) {
    if (!identical(event$event_type, "dispatch_check")) return(list())
    list(battery_pct = max(0, as.numeric(entity$as_list("battery_pct")$battery_pct) - 5))
  },
  stop = function(entity, event, ctx = NULL) entity$last_time >= 4
)

eng <- Engine$new(provider = list(load = function(model_spec, ...) toy_bundle))

entities <- lapply(1:3, function(i) {
  Entity$new(
    init = list(route_zone = "urban", battery_pct = 100 - i),
    schema = schema,
    entity_type = "courier",
    time0 = 0
  )
})

out <- run_cohort(
  eng,
  entities,
  n_param_draws = 2,
  n_sims = 2,
  max_events = 50,
  backend = "none"
)
nrow(out$index)

}

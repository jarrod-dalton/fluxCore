# Plumber API stub for patientSimCore scaffold (Engine owns model materialization)
# To run:
#   plumber::plumb("inst/plumber/sim_api.R") |> pr_run(port = 8000)

library(plumber)
library(patientSimCore)

.store <- new.env(parent = emptyenv())

#* Create a new patient and engine
#* @post /sim/create
function(age0 = 55, miles0 = 10, time0 = 0) {
  id <- paste0("sim_", as.integer(Sys.time()))

  p <- Patient$new(
    init = list(age = as.numeric(age0), miles_to_work = as.numeric(miles0)),
    schema = default_patient_schema(),
    time0 = as.numeric(time0)
  )

  eng <- Engine$new(
    provider   = PackageProvider$new(),
    model_spec = list(name = "default")
  )

  .store[[id]] <- list(patient = p, engine = eng)
  list(id = id, state = p$state(c("age","miles_to_work")), events = p$events)
}

#* Step one event
#* @post /sim/<id>/step
function(id) {
  obj <- .store[[id]]
  if (is.null(obj)) stop("Unknown sim id")

  out <- obj$engine$run(obj$patient, max_events = 1, return_observations = TRUE)

  .store[[id]] <- list(patient = out$patient, engine = obj$engine)
  list(state = out$patient$state(c("age","miles_to_work")), last_event = tail(out$events, 1))
}

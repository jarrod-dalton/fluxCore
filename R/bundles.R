NULL

default_model_bundle <- function(terminal_event_type = "death") {
  terminal_event_type <- as.character(terminal_event_type)

  propose_one <- function(patient, ctx = NULL) {
    t0 <- patient$last_time
    # toy hazard: visits recur, death occurs when age large enough
    dt <- stats::rexp(1, rate = 0.2)
    age <- patient$as_list(c("age"))$age
    event_type <- if (!is.na(age) && age >= 80) terminal_event_type else "VISIT"
    list(time_next = t0 + dt, event_type = event_type)
  }

  propose_events <- function(patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    # single process id "default"
    pid <- "default"
    if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
    ev <- propose_one(patient, ctx = ctx)
    ev$process_id <- pid
    list(default = ev)
  }

  transition <- function(patient, event, ctx = NULL) {
    if (identical(event$event_type, "VISIT")) {
      s <- patient$as_list(c("age", "miles_to_work"))
      # tiny age increment proportional to time advance
      list(age = s$age + (event$time_next - patient$last_time))
    } else {
      NULL
    }
  }

  stop <- function(patient, event, ctx = NULL) {
    identical(event$event_type, terminal_event_type)
  }

  observe <- function(patient, event, ctx = NULL) {
    s <- patient$as_list(c("age", "miles_to_work"))
    data.frame(
      time = patient$last_time,
      event_type = event$event_type,
      age = s$age,
      miles_to_work = s$miles_to_work
    )
  }

  refresh_rules <- function(patient, last_event, changes, ctx = NULL) {
    # default: refresh all processes after any event
    "ALL"
  }

  list(
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    observe = observe,
    refresh_rules = refresh_rules
  )
}

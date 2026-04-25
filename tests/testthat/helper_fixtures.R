default_entity_schema <- function() {
  list(
    alive = list(
      type = "binary",
      levels = c("0", "1"),
      default = TRUE,
      coerce = as.logical,
      validate = function(x) length(x) == 1L && (is.na(x) || is.logical(x))
    ),
    active_followup = list(
      type = "binary",
      levels = c("0", "1"),
      default = TRUE,
      coerce = as.logical,
      validate = function(x) length(x) == 1L && (is.na(x) || is.logical(x))
    )
  )
}

test_model_bundle <- function(terminal_event_type = "death") {
  terminal_event_type <- as.character(terminal_event_type)

  propose_events <- function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    pid <- "default"
    if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
    t0 <- entity$last_time
    dt <- stats::rexp(1, rate = 0.2)
    event_type <- if (stats::runif(1) < 0.05) terminal_event_type else "VISIT"
    list(default = list(time_next = t0 + dt, event_type = event_type, process_id = pid))
  }

  transition <- function(entity, event, ctx = NULL) {
    if (!identical(event$event_type, terminal_event_type)) return(list())
    updates <- list()
    if ("alive" %in% names(entity$current)) updates$alive <- FALSE
    if ("active_followup" %in% names(entity$current)) updates$active_followup <- FALSE
    updates
  }

  stop <- function(entity, event, ctx = NULL) {
    identical(event$event_type, terminal_event_type)
  }

  observe <- function(entity, event, ctx = NULL) {
    out <- list(time = entity$last_time, event_type = event$event_type)
    if ("alive" %in% names(entity$current)) out$alive <- entity$current$alive
    if ("active_followup" %in% names(entity$current)) out$active_followup <- entity$current$active_followup
    as.data.frame(out, stringsAsFactors = FALSE)
  }

  list(
    time_spec = time_spec(unit = "years"),
    event_catalog = c("VISIT", terminal_event_type),
    terminal_events = terminal_event_type,
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    observe = observe,
    refresh_rules = function(entity, last_event, changes, ctx = NULL) "ALL"
  )
}

test_package_provider <- function() {
  PackageProvider$new(registry = list(default = test_model_bundle))
}

#' Patient: schema-based R6 class for event-sourced state with implicit event index `j`
#'
#' The `Patient` class stores a patient's evolving state across simulation events.
#' It is designed for simulations where:
#'
#' - there may be **many state attributes** (e.g., 50+),
#' - only a subset changes at any event (sparse updates),
#' - event times are **non-uniform**, and
#' - events have **types** (including terminal events like `"death"`).
#'
#' ## Key design choice: `j` is implicit (auto-incremented)
#'
#' You do **not** pass `j` to `$update()`. Instead, the class maintains `last_j`
#' internally and assigns the next event index as `j_next = last_j + 1`.
#'
#' ## No-update events
#'
#' Use `patient$update(time, event_type, changes = NULL)` to record an event that does
#' not alter state. The event log advances, but cached state and sparse history are
#' unchanged.
#'
#' @section Fields:
#' - `schema` : Named list. Attribute definitions (`default`, `coerce`, `validate`).
#' - `current` : Named list. Cached current values for all attributes in schema.
#' - `hist` : Named list. Sparse history per attribute (`$j`, `$v`).
#' - `last_j` : Integer. Last event index recorded.
#' - `last_time` : Numeric. Last event time recorded.
#' - `events` : Data frame. Event log with columns `j`, `time`, `event_type`.
#'
#' @section Methods:
#' - `initialize(init, schema = default_patient_schema(), time0 = 0, event_type0 = "init")`
#' - `state(vars = NULL)` : Current state as named numeric vector (subsettable).
#' - `as_list(vars = NULL)` : Current state as list (subsettable).
#' - `update(time, event_type, changes = NULL)` : Record event + sparse updates (**auto `j`**).
#' - `state_at(j, vars = NULL)` : Reconstruct state at event `j` (subsettable).
#'
#' @field schema Named list. Variable definitions (`default`, `coerce`, `validate`).
#' @field current Named list. Current values for all variables in `schema`.
#' @field hist Named list. Sparse history per variable.
#' @field last_j Integer. Last event index recorded.
#' @field last_time Numeric. Last event time recorded.
#' @field events Data frame. Event log with columns `j`, `time`, `event_type`.
#'
#' @param init Named list. Initial values for patient variables.
#' @param schema Named list. Variable definitions; see [default_patient_schema()].
#' @param time0 Numeric. Initial time.
#' @param event_type0 Character. Initial event label (default `"init"`).
#' @param vars Character vector of variable names to return; `NULL` returns all.
#' @param time Numeric. Event time for an update.
#' @param event_type Character. Event label for an update.
#' @param changes Named list of sparse updates; `NULL` means record event with no state changes.
#' @param j Integer. Event index used by `state_at()`.
#'
#' @examples
#' library(patientSimCore)
#' set.seed(42)
#'
#' schema <- default_patient_schema()
#' p <- Patient$new(init = list(age = 55, miles_to_work = 10), schema = schema, time0 = 0)
#'
#' p$update(time = 0.3, event_type = "visit", changes = list(age = 55.3))
#' p$update(time = 0.9, event_type = "check", changes = NULL)
#'
#' p$state(c("age", "miles_to_work"))
#' p$state_at(2, vars = c("age", "miles_to_work"))
#'
#' @export
Patient <- R6::R6Class(
  classname = "Patient",
  public = list(
    schema = NULL,
    current = NULL,
    hist = NULL,
    last_j = NULL,
    last_time = NULL,
    events = NULL,

    initialize = function(init,
                          schema = default_patient_schema(),
                          time0 = 0,
                          event_type0 = "init") {

      .validate_schema(schema)

      time0 <- as.numeric(time0)
      if (length(time0) != 1L || !is.finite(time0)) stop("time0 must be a finite numeric scalar.")
      event_type0 <- .validate_event_type(event_type0)

      self$schema  <- schema
      self$current <- .init_state_from_schema(schema, init)
      self$hist    <- .init_hist_from_state(self$current)

      self$last_j    <- 0L
      self$last_time <- time0

      self$events <- data.frame(
        j = 0L,
        time = time0,
        event_type = as.character(event_type0),
        stringsAsFactors = FALSE
      )

      invisible(self)
    },

    state = function(vars = NULL) {
      if (is.null(vars)) {
        vars <- names(self$current)
      } else {
        vars <- as.character(vars)
        extras <- setdiff(vars, names(self$current))
        if (length(extras) > 0) stop(sprintf("Unknown vars requested: %s", paste(extras, collapse = ", ")))
      }
      unlist(self$current[vars], use.names = TRUE)
    },

    as_list = function(vars = NULL) {
      if (is.null(vars)) return(self$current)
      vars <- as.character(vars)
      extras <- setdiff(vars, names(self$current))
      if (length(extras) > 0) stop(sprintf("Unknown vars requested: %s", paste(extras, collapse = ", ")))
      self$current[vars]
    },

    update = function(time, event_type, changes = NULL) {
      time <- .validate_event_time(time, self$last_time)
      event_type <- .validate_event_type(event_type)

      j_next <- as.integer(self$last_j) + 1L

      self$events <- rbind(
        self$events,
        data.frame(j = j_next, time = time, event_type = event_type, stringsAsFactors = FALSE)
      )

      self$last_j <- j_next
      self$last_time <- time

      res <- .apply_changes(self$current, self$hist, self$schema, j_next, changes)
      self$current <- res$current
      self$hist <- res$hist

      invisible(self)
    },

    state_at = function(j, vars = NULL) {
      j <- as.integer(j)
      if (!is.finite(j) || length(j) != 1L) stop("j must be a finite integer scalar.")
      if (j < 0L) stop("j must be >= 0.")
      if (j > self$last_j) stop("j cannot exceed patient$last_j.")

      if (is.null(vars)) {
        vars <- names(self$schema)
      } else {
        vars <- as.character(vars)
        extras <- setdiff(vars, names(self$schema))
        if (length(extras) > 0) stop(sprintf("Unknown vars requested: %s", paste(extras, collapse = ", ")))
      }

      out <- numeric(length(vars))
      names(out) <- vars

      for (k in vars) {
        jj <- self$hist[[k]]$j
        vv <- self$hist[[k]]$v
        idx <- findInterval(j, jj)
        out[[k]] <- vv[[idx]]
      }
      out
    }
  )
)

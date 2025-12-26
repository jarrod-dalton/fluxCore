
.eval_derived_vars <- function(derived_vars, patient, j, t) {
  if (is.null(derived_vars) || length(derived_vars) == 0L) return(list())
  nms <- names(derived_vars)
  if (is.null(nms) || any(nms == "")) stop("All derived_vars must be a named list.")
  out <- list()
  for (nm in nms) {
    f <- derived_vars[[nm]]
    if (!is.function(f)) stop(sprintf("derived_vars[['%s']] must be a function", nm))
    val <- f(patient, j = j, t = t)
    if (!is.null(val)) out[[nm]] <- val
  }
  out
}

Patient <- R6::R6Class(
  classname = "Patient",
  public = list(
    schema = NULL,
    current = NULL,
    hist = NULL,
    last_j = NULL,
    last_time = NULL,
    events = NULL,
    derived_vars = NULL,

    initialize = function(init,
                          schema = default_patient_schema(),
                          derived_vars = NULL,
                          time0 = 0,
                          event_type0 = "init") {

      schema <- .validate_schema(schema)
      self$schema <- schema

      time0 <- as.numeric(time0)
      if (length(time0) != 1L || !is.finite(time0)) stop("time0 must be a finite numeric scalar.")
      event_type0 <- .validate_event_type(event_type0)

      self$schema  <- schema
      if (is.null(derived_vars)) derived_vars <- list()
      self$derived_vars <- derived_vars
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
      self$current[vars]
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

state_at_time = function(time, vars = NULL) {
  if (length(time) != 1 || !is.finite(time)) stop("`time` must be a single finite numeric value")
  time <- as.numeric(time)
  t0 <- self$events$time[1]
  if (time < t0) stop(sprintf("`time` must be >= time0 (%.6g)", t0))
  pos <- findInterval(time, self$events$time)
  if (pos <= 0) stop(sprintf("No event time <= %.6g found", time))
  j_star <- self$events$j[pos]
  self$state_at(j_star, vars = vars)
},

snapshot = function(vars = NULL) {
  base <- self$as_list(vars = NULL)
  d <- .eval_derived_vars(self$derived_vars, self, j = self$last_j, t = self$last_time)
  if (length(intersect(names(base), names(d))) > 0) stop("Derived variable name collides with base state variable name.")
  snap <- c(base, d)
  if (!is.null(vars)) snap <- snap[vars]
  snap
},

snapshot_at = function(j, vars = NULL) {
  j <- as.integer(j)
  if (length(j) != 1 || !is.finite(j)) stop("j must be a finite integer scalar.")
  if (j < 0L) stop("j must be >= 0.")
  if (j > self$last_j) stop("j cannot exceed patient$last_j.")
  t <- self$events$time[match(j, self$events$j)]
  base <- self$state_at(j, vars = NULL)
  d <- .eval_derived_vars(self$derived_vars, self, j = j, t = t)
  if (length(intersect(names(base), names(d))) > 0) stop("Derived variable name collides with base state variable name.")
  snap <- c(as.list(base), d)
  if (!is.null(vars)) snap <- snap[vars]
  snap
},

snapshot_at_time = function(time, vars = NULL) {
  if (length(time) != 1 || !is.finite(time)) stop("`time` must be a single finite numeric value")
  time <- as.numeric(time)
  t0 <- self$events$time[1]
  if (time < t0) stop(sprintf("`time` must be >= time0 (%.6g)", t0))
  pos <- findInterval(time, self$events$time)
  if (pos <= 0) stop(sprintf("No event time <= %.6g found", time))
  j_star <- self$events$j[pos]
  base <- self$state_at(j_star, vars = NULL)
  d <- .eval_derived_vars(self$derived_vars, self, j = j_star, t = time)
  if (length(intersect(names(base), names(d))) > 0) stop("Derived variable name collides with base state variable name.")
  snap <- c(as.list(base), d)
  if (!is.null(vars)) snap <- snap[vars]
  snap
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

      out <- vector("list", length(vars))
      names(out) <- vars

      for (k in vars) {
        jj <- self$hist[[k]]$j
        vv <- self$hist[[k]]$v
        idx <- findInterval(j, jj)
        out[[k]] <- vv[[idx]]
      }
      out
    }
  ),
  active = list(
    j = function(value) {
      if (missing(value)) return(self$last_j)
      stop("`j` is read-only. It is maintained internally; call `$update()` to advance events.")
    }
  )
)

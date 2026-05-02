
.eval_derived_vars <- function(derived_vars, entity, j, t) {
  if (is.null(derived_vars) || length(derived_vars) == 0L) return(list())
  nms <- names(derived_vars)
  if (is.null(nms) || any(nms == "")) stop("All derived_vars must be a named list.")
  out <- list()
  for (nm in nms) {
    f <- derived_vars[[nm]]
    if (!is.function(f)) stop(sprintf("derived_vars[['%s']] must be a function", nm))
    val <- f(entity, j = j, t = t)
    if (!is.null(val)) out[[nm]] <- val
  }
  out
}

#' Entity
#'
#' R6 simulation state container used by fluxCore::Engine. An Entity
#' stores current state, event history, derived-variable registrations, and metadata
#' such as id and optional meta$entity_type.
#'
#' @importFrom R6 R6Class
#' @export
Entity <- R6::R6Class(
  classname = "Entity",
  public = list(
    schema = NULL,
    current = NULL,
    hist = NULL,
    last_j = NULL,
    last_time = NULL,
    events = NULL,
    derived_vars = NULL,
                          id = NULL,
    meta = NULL,

    initialize = function(init = list(),
                          schema,
                          derived_vars = NULL,
                          id = NULL,
                          entity_type = NULL,
                          time0 = 0,
                          event_type0 = "init") {

      if (missing(schema) || is.null(schema)) {
        stop("schema is required; define an explicit state schema when constructing Entity.", call. = FALSE)
      }

      # Accept common row-like inputs:
      # - named list (preferred)
      # - named atomic vector
      # - 1-row data.frame
      if (is.data.frame(init)) {
        if (nrow(init) != 1L) stop("If init is a data.frame it must have exactly one row.", call. = FALSE)
        init <- as.list(init[1, , drop = FALSE])
      } else if (!is.list(init) && !is.null(init)) {
        if (is.atomic(init) && !is.null(names(init))) {
          init <- as.list(init)
        } else {
          stop("init must be a named list, a named atomic vector, a 1-row data.frame, or NULL.", call. = FALSE)
        }
      }

      if (is.null(init)) init <- list()

      schema <- .validate_schema(schema)
      self$schema <- schema

      time0 <- as.numeric(time0)
      if (length(time0) != 1L || !is.finite(time0)) stop("time0 must be a finite numeric scalar.")
      event_type0 <- .validate_event_type(event_type0)

      self$schema  <- schema
      if (is.null(derived_vars)) derived_vars <- list()
      self$derived_vars <- derived_vars

      if (!is.null(id)) {
        if (length(id) != 1L) stop("id must be NULL or a length-1 scalar.")
        id <- as.character(id)
      }
      self$id <- id

      if (!is.null(entity_type)) {
        if (length(entity_type) != 1L) stop("entity_type must be NULL or a length-1 scalar.")
        entity_type <- as.character(entity_type)
        if (identical(entity_type, "") || is.na(entity_type)) {
          stop("entity_type must be a non-empty, non-missing scalar when provided.")
        }
      }

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

      self$meta <- list()
      if (!is.null(entity_type)) self$meta$entity_type <- entity_type

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
      .as_state(self$current[vars])
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
  if (j > self$last_j) stop("j cannot exceed entity$last_j.")
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
      if (j > self$last_j) stop("j cannot exceed entity$last_j.")

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

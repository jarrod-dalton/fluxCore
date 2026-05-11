## Tests for DecisionPoint action_handlers

# --- Helpers ------------------------------------------------------------------

.make_handler_bundle <- function() {
  propose_events <- function(entity, process_ids = NULL, current_proposals = NULL) {
    pid <- "default"
    if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
    t0 <- entity$last_time
    list(default = list(time_next = t0 + 1, event_type = "trigger_event", process_id = pid))
  }

  transition <- function(entity, event) {
    # Should NOT be called for action events when handler is present
    if (event$event_type %in% c("do_action", "alt_action")) {
      return(list(transition_called = TRUE))
    }
    list()
  }

  stop_fn <- function(entity, event) isTRUE(entity$last_time >= 5)

  list(
    time_spec      = time_spec(unit = "years"),
    event_catalog  = c("trigger_event"),
    propose_events = propose_events,
    transition     = transition,
    stop           = stop_fn,
    observe        = NULL,
    refresh_rules  = function(entity, last_event, changes) "ALL"
  )
}

.make_handler_schema <- function(dp) {
  list(
    variables = list(
      score = list(type = "numeric", default = 0, coerce = as.numeric,
                   validate = function(x) length(x) == 1L && is.finite(x)),
      transition_called = list(type = "binary", levels = c("0","1"), default = FALSE,
                               coerce = as.logical, validate = function(x) length(x) == 1L && is.logical(x))
    ),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
}

# --- action_handlers: handler called instead of transition --------------------

test_that("action_handlers: handler is called instead of bundle$transition()", {
  dp <- DecisionPoint(
    id              = "test_dp",
    trigger         = "trigger_event",
    allowed_actions = c("do_action"),
    action_handlers = list(
      do_action = function(entity, event) {
        list(score = as.numeric(entity$current$score) + 10)
      }
    )
  )
  schema <- .make_handler_schema(dp)
  policy <- function(decision_point, entity) {
    ActionEvent(action_type = "do_action", time_next = entity$last_time + 0.01)
  }

  engine <- load_model(schema = schema, bundle = .make_handler_bundle(),
                       policy = policy, trajectory = list(detail = "summary"))
  entity <- Entity$new(schema = schema$variables, init = list(score = 0))
  out <- engine$run(entity = entity, max_events = 20)

  # Handler was called: score should have increased

  expect_true(as.numeric(out$entity$current$score) > 0)
  # Transition was NOT called for the action event
  expect_false(isTRUE(out$entity$current$transition_called))
})

# --- No action_handler: falls through to bundle$transition() ------------------

test_that("no action_handlers: ActionEvent goes through bundle$transition()", {
  dp <- DecisionPoint(
    id              = "test_dp",
    trigger         = "trigger_event",
    allowed_actions = c("do_action")
    # No action_handlers
  )
  schema <- .make_handler_schema(dp)
  bundle <- .make_handler_bundle()
  # Must register "do_action" in event_catalog since no auto-registration without handlers
  bundle$event_catalog <- c(bundle$event_catalog, "do_action")

  policy <- function(decision_point, entity) {
    ActionEvent(action_type = "do_action", time_next = entity$last_time + 0.01)
  }

  engine <- load_model(schema = schema, bundle = bundle,
                       policy = policy, trajectory = list(detail = "summary"))
  entity <- Entity$new(schema = schema$variables, init = list(score = 0))
  out <- engine$run(entity = entity, max_events = 20)

  # Transition WAS called for the action event
  expect_true(isTRUE(out$entity$current$transition_called))
})

# --- Auto-registration of action event types ----------------------------------

test_that("action_handlers auto-registers event types (no manual event_catalog needed)", {
  dp <- DecisionPoint(
    id              = "test_dp",
    trigger         = "trigger_event",
    allowed_actions = c("do_action", "alt_action"),
    action_handlers = list(
      do_action  = function(entity, event) list(score = as.numeric(entity$current$score) + 1),
      alt_action = function(entity, event) list(score = as.numeric(entity$current$score) + 100)
    )
  )
  schema <- .make_handler_schema(dp)
  bundle <- .make_handler_bundle()
  # Note: bundle$event_catalog does NOT include "do_action" or "alt_action"

  policy <- function(decision_point, entity) {
    ActionEvent(action_type = "do_action", time_next = entity$last_time + 0.01)
  }

  # Should NOT error about event_type not in event_catalog
  engine <- load_model(schema = schema, bundle = bundle, policy = policy)
  entity <- Entity$new(schema = schema$variables, init = list(score = 0))
  out <- engine$run(entity = entity, max_events = 10)
  expect_true(as.numeric(out$entity$current$score) > 0)
})

# --- Handler with param_ctx ---------------------------------------------------

test_that("action_handlers: param_ctx is passed when handler accepts it", {
  received_ctx <- NULL
  dp <- DecisionPoint(
    id              = "test_dp",
    trigger         = "trigger_event",
    allowed_actions = c("do_action"),
    action_handlers = list(
      do_action = function(entity, event, param_ctx) {
        received_ctx <<- param_ctx
        list(score = 99)
      }
    )
  )
  schema <- .make_handler_schema(dp)
  policy <- function(decision_point, entity) {
    ActionEvent(action_type = "do_action", time_next = entity$last_time + 0.01)
  }

  engine <- load_model(schema = schema, bundle = .make_handler_bundle(), policy = policy)
  entity <- Entity$new(schema = schema$variables, init = list(score = 0))
  engine$run(entity = entity, max_events = 5)

  expect_false(is.null(received_ctx))
})

# --- Validation: handler names must be subset of allowed_actions ---------------

test_that("DecisionPoint errors when action_handler names not in allowed_actions", {
  expect_error(
    DecisionPoint(
      id              = "bad_dp",
      trigger         = "trigger_event",
      allowed_actions = c("accept"),
      action_handlers = list(
        accept  = function(entity, event) list(),
        decline = function(entity, event) list()
      )
    ),
    "not in allowed_actions"
  )
})

# --- Validation: action_handlers must be named list of functions ---------------

test_that("DecisionPoint errors on invalid action_handlers", {
  expect_error(
    DecisionPoint(id = "dp", trigger = "X", action_handlers = list(function(e, ev) list())),
    "named list"
  )
  expect_error(
    DecisionPoint(id = "dp", trigger = "X", action_handlers = list(foo = "not_a_function")),
    "must be a function"
  )
  expect_error(
    DecisionPoint(id = "dp", trigger = "X", action_handlers = list()),
    "non-empty"
  )
})

# --- Handler returning NULL means no state change -----------------------------

test_that("action_handler returning NULL applies no state changes", {
  dp <- DecisionPoint(
    id              = "test_dp",
    trigger         = "trigger_event",
    allowed_actions = c("do_action"),
    action_handlers = list(
      do_action = function(entity, event) NULL
    )
  )
  schema <- .make_handler_schema(dp)
  policy <- function(decision_point, entity) {
    ActionEvent(action_type = "do_action", time_next = entity$last_time + 0.01)
  }

  engine <- load_model(schema = schema, bundle = .make_handler_bundle(), policy = policy)
  entity <- Entity$new(schema = schema$variables, init = list(score = 5))
  out <- engine$run(entity = entity, max_events = 10)

  # Score unchanged
  expect_equal(as.numeric(out$entity$current$score), 5)
})

## Stage 2B tests: policy dispatch through Engine event loop

.make_stage2b_bundle <- function() {
  propose_events <- function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    pid <- "default"
    if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())

    # Deterministic baseline process: VISIT every +1 time unit.
    t0 <- entity$last_time
    list(default = list(time_next = t0 + 1, event_type = "VISIT", process_id = pid))
  }

  transition <- function(entity, event, ctx = NULL) {
    if (identical(event$event_type, "ACT")) return(list(acted = TRUE))
    list()
  }

  stop <- function(entity, event, ctx = NULL) {
    # End run once the action is applied.
    isTRUE(identical(event$event_type, "ACT"))
  }

  list(
    time_spec = time_spec(unit = "years"),
    event_catalog = c("VISIT", "ACT"),
    terminal_events = "ACT",
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    observe = NULL,
    refresh_rules = function(entity, last_event, changes, ctx = NULL) "ALL"
  )
}

.make_stage2b_schema <- function(with_decision_points = TRUE) {
  sch <- list(
    variables = list(
      acted = list(
        type = "binary",
        levels = c("0", "1"),
        default = FALSE,
        coerce = as.logical,
        validate = function(x) length(x) == 1L && is.logical(x)
      )
    ),
    time_spec = time_spec(unit = "years"),
    event_catalog = c("VISIT", "ACT")
  )
  if (isTRUE(with_decision_points)) {
    sch$decision_points <- list(
      DecisionPoint(
        id = "after_visit",
        trigger = "VISIT",
        allowed_actions = "ACT"
      )
    )
  }
  sch
}

test_that("Engine v2: policy action is enqueued and realized after decision point fires", {
  bundle <- .make_stage2b_bundle()
  schema <- .make_stage2b_schema(with_decision_points = TRUE)

  policy <- list(
    propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
      ActionEvent(
        action_type = "ACT",
        time_next = entity$last_time + 0.1,
        decision_point_id = decision_point$id,
        metadata = list(source = "test-policy")
      )
    }
  )

  engine <- suppressWarnings(load_model(schema = schema, bundle = bundle, policy = policy))
  entity <- Entity$new(schema = schema$variables)

  out <- engine$run(entity = entity, max_events = 10)

  expect_true("VISIT" %in% out$events$event_type)
  expect_true("ACT" %in% out$events$event_type)

  idx_visit <- which(out$events$event_type == "VISIT")[1]
  idx_act <- which(out$events$event_type == "ACT")[1]
  expect_true(idx_act > idx_visit)
  expect_true(isTRUE(out$entity$current$acted))
})

test_that("Engine v2: no declared decision points means policy is never called", {
  bundle <- .make_stage2b_bundle()
  schema <- .make_stage2b_schema(with_decision_points = FALSE)

  calls <- 0L
  policy <- list(
    propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
      calls <<- calls + 1L
      NULL
    }
  )

  engine <- suppressWarnings(load_model(schema = schema, bundle = bundle, policy = policy))
  entity <- Entity$new(schema = schema$variables)

  engine$run(entity = entity, max_events = 2)
  expect_equal(calls, 0L)
})

test_that("Engine v2: policy action outside allowed_actions is ignored with warning", {
  bundle <- .make_stage2b_bundle()
  schema <- .make_stage2b_schema(with_decision_points = TRUE)

  policy <- list(
    propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
      ActionEvent(
        action_type = "NOT_ALLOWED",
        time_next = entity$last_time + 0.1,
        decision_point_id = decision_point$id
      )
    }
  )

  engine <- suppressWarnings(load_model(schema = schema, bundle = bundle, policy = policy))
  entity <- Entity$new(schema = schema$variables)

  expect_warning(
    out <- engine$run(entity = entity, max_events = 2),
    "allowed_actions"
  )
  expect_false("NOT_ALLOWED" %in% out$events$event_type)
})

test_that("Engine v2: policy receives sim_ctx and param_ctx when declared", {
  bundle <- .make_stage2b_bundle()
  schema <- .make_stage2b_schema(with_decision_points = TRUE)

  seen <- new.env(parent = emptyenv())
  seen$sim_ctx <- NULL
  seen$param_ctx <- NULL

  policy <- list(
    propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
      seen$sim_ctx <- sim_ctx
      seen$param_ctx <- param_ctx
      ActionEvent(
        action_type = "ACT",
        time_next = entity$last_time + 0.1,
        decision_point_id = decision_point$id
      )
    }
  )

  engine <- suppressWarnings(load_model(schema = schema, bundle = bundle, policy = policy))
  entity <- Entity$new(schema = schema$variables)

  engine$run(entity = entity, max_events = 3)

  expect_true(inherits(seen$sim_ctx, "SimContext"))
  expect_true(inherits(seen$param_ctx, "ParamContext"))
})

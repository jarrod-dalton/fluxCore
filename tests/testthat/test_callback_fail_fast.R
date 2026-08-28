## Decision callback failures must not be converted into valid model outcomes.

.q8_case <- function(condition = function(entity) TRUE,
                     policy = function(decision_point, entity) {
                       ActionEvent("ACCEPT", time_next = entity$last_time + 0.25)
                     },
                     handler = function(entity, event) {
                       list(accepted = as.integer(entity$current$accepted) + 1L)
                     }) {
  dp <- DecisionPoint(
    id = "dispatch_choice",
    trigger = "DISPATCH",
    allowed_actions = "ACCEPT",
    action_handlers = list(ACCEPT = handler),
    condition = condition,
    audit = TRUE,
    on_pending_action = "error"
  )

  schema <- set_schema(
    vars = list(
      dispatches = list(type = "nonnegative_integer", default = 0L),
      accepted = list(type = "nonnegative_integer", default = 0L)
    ),
    time_spec = time_spec(unit = "hours"),
    decision_points = list(dp)
  )

  bundle <- list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = c("DISPATCH", "ACCEPT"),
    propose_events = function(entity) {
      if (entity$last_time >= 1) return(list())
      list(dispatch = list(time_next = 1, event_type = "DISPATCH"))
    },
    transition = function(entity, event) {
      list(dispatches = as.integer(entity$current$dispatches) + 1L)
    },
    stop = function(entity, event) identical(event$event_type, "ACCEPT"),
    refresh_rules = function(entity, last_event, changes) "ALL"
  )

  entity <- Entity$new(schema = schema$variables)
  list(
    entity = entity,
    engine = load_model(
      schema = schema,
      bundle = bundle,
      policy = list(propose_action = policy),
      trajectory = list(detail = "none")
    )
  )
}

.expect_q8_trigger_committed <- function(entity) {
  expect_identical(entity$current$dispatches, 1L)
  expect_identical(entity$current$accepted, 0L)
  expect_equal(entity$last_time, 1)
  expect_equal(entity$events$event_type, c("init", "DISPATCH"))
}

test_that("condition callback errors fail with decision-point context", {
  case <- .q8_case(condition = function(entity) stop("condition exploded"))

  expect_error(
    suppressWarnings(case$engine$run(case$entity, max_events = 3)),
    "DecisionPoint\\('dispatch_choice'\\) condition callback errored: condition exploded"
  )
  .expect_q8_trigger_committed(case$entity)
})

test_that("policy callback errors fail with decision-point context", {
  case <- .q8_case(
    policy = function(decision_point, entity) stop("policy exploded")
  )

  expect_error(
    suppressWarnings(case$engine$run(case$entity, max_events = 3)),
    "policy\\$propose_action\\(\\) errored for DecisionPoint\\('dispatch_choice'\\): policy exploded"
  )
  .expect_q8_trigger_committed(case$entity)
})

test_that("action-handler errors fail before the action is committed", {
  case <- .q8_case(
    handler = function(entity, event) stop("handler exploded")
  )

  expect_error(
    suppressWarnings(case$engine$run(case$entity, max_events = 3)),
    "action_handler for action_type 'ACCEPT' from DecisionPoint\\('dispatch_choice'\\) errored: handler exploded"
  )
  .expect_q8_trigger_committed(case$entity)
})

invalid_conditions <- list(
  zero_length = function(entity) logical(),
  multiple_values = function(entity) c(TRUE, FALSE),
  missing_value = function(entity) NA,
  non_logical = function(entity) 1
)

for (condition_name in names(invalid_conditions)) {
  test_that(sprintf("condition rejects %s results", condition_name), {
    case <- .q8_case(condition = invalid_conditions[[condition_name]])

    expect_error(
      case$engine$run(case$entity, max_events = 3),
      "DecisionPoint\\('dispatch_choice'\\) condition callback must return exactly one non-missing logical value"
    )
    .expect_q8_trigger_committed(case$entity)
  })
}

test_that("an intentional FALSE condition remains a policy veto", {
  policy_calls <- 0L
  case <- .q8_case(
    condition = function(entity) FALSE,
    policy = function(decision_point, entity) {
      policy_calls <<- policy_calls + 1L
      NULL
    }
  )

  out <- case$engine$run(case$entity, max_events = 3)

  expect_identical(policy_calls, 0L)
  expect_equal(out$events$event_type, c("init", "DISPATCH"))
  expect_identical(out$entity$current$dispatches, 1L)
  expect_identical(out$entity$current$accepted, 0L)
})

test_that("an intentional NULL policy result remains no action", {
  case <- .q8_case(policy = function(decision_point, entity) NULL)

  out <- case$engine$run(case$entity, max_events = 3)

  expect_equal(out$events$event_type, c("init", "DISPATCH"))
  expect_identical(out$entity$current$dispatches, 1L)
  expect_identical(out$entity$current$accepted, 0L)
})

test_that("an intentional NULL handler result realizes a no-effect action", {
  case <- .q8_case(handler = function(entity, event) NULL)

  out <- case$engine$run(case$entity, max_events = 3)

  expect_equal(out$events$event_type, c("init", "DISPATCH", "ACCEPT"))
  expect_equal(out$events$time, c(0, 1, 1.25))
  expect_identical(out$entity$current$dispatches, 1L)
  expect_identical(out$entity$current$accepted, 0L)
})

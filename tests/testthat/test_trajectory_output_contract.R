test_that("trajectory_table retains identity and names policy selection accurately", {
  selected <- ActionEvent(
    action_type = "ASSIGN_COURIER",
    time_next = 3,
    decision_point_id = "dispatch"
  )
  record <- TrajectoryRecord(
    run_id = "run_7",
    entity_id = "courier_3",
    t = 2,
    decision_point_id = "dispatch",
    observation = list(open_orders = 2L),
    realized_event = list(event_type = "ORDER_READY"),
    selected_action = selected,
    state_before = list(open_orders = 1L),
    state_after = list(open_orders = 2L),
    condition_met = TRUE
  )

  out <- trajectory_table(list(record), vars = "open_orders")

  expect_identical(
    names(out),
    c(
      "run_id", "entity_id", "t", "decision_point_id", "trigger_event",
      "selected_action", "condition_met", "open_orders_before", "open_orders_after"
    )
  )
  expect_identical(out$run_id, "run_7")
  expect_identical(out$entity_id, "courier_3")
  expect_identical(out$trigger_event, "ORDER_READY")
  expect_identical(out$selected_action, "ASSIGN_COURIER")
  expect_false("action_taken" %in% names(out))
})

test_that("empty trajectory_table has the same fixed schema", {
  base <- trajectory_table(list())
  expect_identical(
    names(base),
    c(
      "run_id", "entity_id", "t", "decision_point_id", "trigger_event",
      "selected_action", "condition_met"
    )
  )

  out <- trajectory_table(list(), vars = "open_orders")

  expect_identical(
    names(out),
    c(
      "run_id", "entity_id", "t", "decision_point_id", "trigger_event",
      "selected_action", "condition_met", "open_orders_before", "open_orders_after"
    )
  )
  expect_identical(nrow(out), 0L)
  expect_type(out$run_id, "character")
  expect_type(out$entity_id, "character")
  expect_type(out$t, "double")
  expect_type(out$selected_action, "character")
  expect_type(out$condition_met, "logical")
})

test_that("selected_action precedes pending keep resolution", {
  dp <- DecisionPoint(
    id = "dispatch",
    trigger = "ORDER_READY",
    allowed_actions = c("KEEP_FIRST", "DISCARD_SECOND"),
    action_handlers = list(
      KEEP_FIRST = function(entity, event) list(),
      DISCARD_SECOND = function(entity, event) list()
    ),
    on_pending_action = "keep"
  )
  schema <- list(
    variables = list(n_ready = list(type = "count", default = 0L)),
    time_spec = time_spec(unit = "hours"),
    event_catalog = c("ORDER_READY", "KEEP_FIRST", "DISCARD_SECOND"),
    decision_points = list(dp)
  )
  bundle <- list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = c("ORDER_READY", "KEEP_FIRST", "DISCARD_SECOND"),
    propose_events = function(entity) {
      n <- as.integer(entity$current$n_ready)
      if (n >= 2L) return(list())
      list(order = list(time_next = n + 1, event_type = "ORDER_READY"))
    },
    transition = function(entity, event) {
      if (identical(event$event_type, "ORDER_READY")) {
        return(list(n_ready = as.integer(entity$current$n_ready) + 1L))
      }
      list()
    },
    stop = function(entity, event) {
      event$event_type %in% c("KEEP_FIRST", "DISCARD_SECOND")
    }
  )
  policy <- list(propose_action = function(decision_point, entity) {
    action_type <- if (entity$current$n_ready == 1L) "KEEP_FIRST" else "DISCARD_SECOND"
    ActionEvent(action_type, time_next = entity$last_time + 5)
  })
  engine <- load_model(
    schema = schema,
    bundle = bundle,
    policy = policy,
    trajectory = list(detail = "none")
  )

  out <- engine$run(Entity$new(schema = schema$variables), max_events = 4)
  selections <- vapply(
    out$trajectory_records,
    function(record) record$selected_action$action_type,
    character(1)
  )

  expect_identical(selections, c("KEEP_FIRST", "DISCARD_SECOND"))
  expect_identical(
    out$events$event_type[out$events$event_type %in% c("KEEP_FIRST", "DISCARD_SECOND")],
    "KEEP_FIRST"
  )
  expect_identical(out$events$time[out$events$event_type == "KEEP_FIRST"], 6)
})

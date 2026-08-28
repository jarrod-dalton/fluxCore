.s3c_vars <- function() {
  list(checks = list(type = "nonnegative_integer", default = 0L))
}

.s3c_schema <- function(decision_points, decision_groups = NULL) {
  set_schema(
    vars = .s3c_vars(),
    time_spec = time_spec(unit = "hours"),
    decision_points = decision_points,
    decision_groups = decision_groups
  )
}

.s3c_entity <- function(schema, id = "courier_1") {
  Entity$new(schema = schema$variables, id = id)
}

.s3c_bundle <- function(n_checks = 1L,
                        event_catalog = c("CHECK", "A", "C")) {
  list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = event_catalog,
    propose_events = function(entity) {
      if (entity$current$checks >= n_checks) return(list())
      list(
        check = list(
          time_next = entity$last_time + 1,
          event_type = "CHECK"
        )
      )
    },
    transition = function(entity, event) {
      if (identical(event$event_type, "CHECK")) {
        list(checks = entity$current$checks + 1L)
      } else {
        list()
      }
    },
    stop = function(entity, event) FALSE
  )
}

.s3c_null_plan <- function(eligible_decision_points, metadata = NULL) {
  selections <- rep(list(NULL), length(eligible_decision_points))
  names(selections) <- names(eligible_decision_points)
  DecisionPlan(selections, metadata = metadata)
}


# TrajectoryRecord surface -------------------------------------------------

test_that("TrajectoryRecord appends grouped fields without moving legacy reward", {
  positional <- TrajectoryRecord(
    "run_1", "courier_1", 1, "ordinary", list(), list(),
    NULL, NULL, NULL, NULL, NULL, TRUE, 42
  )

  expect_identical(positional$reward, 42)
  expect_null(positional$grouped_decision_point_id)
  expect_null(positional$group_activation_id)
  expect_null(positional$decision_plan_metadata)

  grouped <- TrajectoryRecord(
    run_id = "run_1",
    entity_id = "courier_1",
    t = 1,
    decision_point_id = "dispatch",
    observation = list(),
    realized_event = list(event_type = "CHECK"),
    grouped_decision_point_id = "post_dispatch_review",
    group_activation_id = "group_activation_1",
    decision_plan_metadata = list(strategy = "battery_first")
  )

  expect_identical(grouped$grouped_decision_point_id, "post_dispatch_review")
  expect_identical(grouped$group_activation_id, "group_activation_1")
  expect_identical(grouped$decision_plan_metadata, list(strategy = "battery_first"))
  expect_output(print(grouped), "grouped_decision_point_id: post_dispatch_review")
  expect_output(print(grouped), "decision_plan_metadata.*strategy")
  ordinary_print <- capture.output(print(positional))
  expect_false(any(grepl("grouped_decision_point_id", ordinary_print, fixed = TRUE)))
})

test_that("TrajectoryRecord validates grouped identity and plan metadata together", {
  base_args <- list(
    run_id = "run_1",
    entity_id = "courier_1",
    t = 1,
    decision_point_id = "dispatch",
    observation = list(),
    realized_event = list(event_type = "CHECK")
  )

  expect_error(
    do.call(TrajectoryRecord, c(base_args, list(grouped_decision_point_id = "group"))),
    "must be supplied together"
  )
  expect_error(
    do.call(
      TrajectoryRecord,
      c(base_args, list(
        grouped_decision_point_id = "group",
        group_activation_id = ""
      ))
    ),
    "non-empty character scalar"
  )
  expect_error(
    do.call(
      TrajectoryRecord,
      c(base_args, list(decision_plan_metadata = list(strategy = "joint")))
    ),
    "requires.*grouped_decision_point_id"
  )
  expect_error(
    do.call(
      TrajectoryRecord,
      c(base_args, list(
        grouped_decision_point_id = "group",
        group_activation_id = "group_activation_1",
        decision_plan_metadata = "joint"
      ))
    ),
    "named list or NULL"
  )
  expect_error(
    do.call(
      TrajectoryRecord,
      c(base_args, list(
        grouped_decision_point_id = "group",
        group_activation_id = "group_activation_1",
        decision_plan_metadata = list("joint")
      ))
    ),
    "one non-empty name"
  )
  expect_error(
    do.call(
      TrajectoryRecord,
      c(base_args, list(
        grouped_decision_point_id = "group",
        group_activation_id = "group_activation_1",
        decision_plan_metadata = stats::setNames(list(1, 2), c("score", "score"))
      ))
    ),
    "names must be unique"
  )
})

test_that("trajectory_table binds legacy and grouped records with compact identity only", {
  legacy <- list(
    run_id = "run_legacy",
    entity_id = "courier_legacy",
    t = 1,
    decision_point_id = "ordinary",
    observation = list(),
    realized_event = list(event_type = "CHECK"),
    candidate_actions = NULL,
    proposed_actions = list(),
    selected_action = NULL,
    state_before = list(checks = 0L),
    state_after = list(checks = 1L),
    condition_met = TRUE,
    reward = NULL
  )
  grouped <- TrajectoryRecord(
    run_id = "run_new",
    entity_id = "courier_new",
    t = 1,
    decision_point_id = "dispatch",
    observation = list(),
    realized_event = list(event_type = "CHECK"),
    state_before = list(checks = 0L),
    state_after = list(checks = 1L),
    condition_met = TRUE,
    grouped_decision_point_id = "post_dispatch_review",
    group_activation_id = "group_activation_1",
    decision_plan_metadata = list(strategy = "joint")
  )

  table <- trajectory_table(list(legacy, grouped), vars = "checks")

  expect_identical(nrow(table), 2L)
  expect_type(table$grouped_decision_point_id, "character")
  expect_type(table$group_activation_id, "character")
  expect_true(is.na(table$grouped_decision_point_id[[1L]]))
  expect_true(is.na(table$group_activation_id[[1L]]))
  expect_identical(table$grouped_decision_point_id[[2L]], "post_dispatch_review")
  expect_identical(table$group_activation_id[[2L]], "group_activation_1")
  expect_false("decision_plan_metadata" %in% names(table))
})


# Engine grouped audit rows ------------------------------------------------

test_that("grouped trajectories use declared leaf order and share raw plan audit data", {
  leaves <- list(
    DecisionPoint(
      "a", NULL,
      allowed_actions = "A",
      condition = function(entity) TRUE
    ),
    DecisionPoint(
      "b", NULL,
      condition = function(entity) FALSE,
      audit = TRUE
    ),
    DecisionPoint(
      "c", NULL,
      allowed_actions = "C",
      condition = function(entity) TRUE
    ),
    DecisionPoint(
      "d", NULL,
      condition = function(entity) TRUE
    ),
    DecisionPoint(
      "e", NULL,
      condition = function(entity) FALSE,
      audit = FALSE
    )
  )
  group <- GroupedDecisionPoint(
    "post_dispatch_review", "CHECK", c("a", "b", "c", "d", "e")
  )
  schema <- .s3c_schema(leaves, list(group))
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      expect_identical(names(eligible_decision_points), c("a", "c", "d"))
      DecisionPlan(
        list(
          d = NULL,
          c = ActionEvent("C", entity$last_time + 10),
          a = ActionEvent("A", entity$last_time + 10)
        ),
        metadata = list(strategy = "battery_first", joint_score = 0.83)
      )
    }
  )
  engine <- load_model(
    schema,
    .s3c_bundle(),
    policy = policy,
    trajectory = list(detail = "full")
  )

  out <- engine$run(.s3c_entity(schema), max_events = 1)
  records <- out$trajectory_records

  expect_identical(length(records), 4L)
  expect_identical(
    vapply(records, `[[`, character(1), "decision_point_id"),
    c("a", "b", "c", "d")
  )
  expect_identical(
    vapply(records, `[[`, logical(1), "condition_met"),
    c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(records[[1L]]$selected_action$action_type, "A")
  expect_null(records[[2L]]$selected_action)
  expect_identical(records[[3L]]$selected_action$action_type, "C")
  expect_null(records[[4L]]$selected_action)
  expect_true(all(vapply(
    records,
    function(record) identical(
      record$grouped_decision_point_id,
      "post_dispatch_review"
    ),
    logical(1)
  )))
  expect_true(all(vapply(
    records,
    function(record) identical(record$group_activation_id, "group_activation_1"),
    logical(1)
  )))
  expect_true(all(vapply(
    records,
    function(record) identical(
      record$decision_plan_metadata,
      list(strategy = "battery_first", joint_score = 0.83)
    ),
    logical(1)
  )))
  expect_true(all(vapply(records, function(record) record$state_before$checks == 0L, logical(1))))
  expect_true(all(vapply(records, function(record) record$state_after$checks == 1L, logical(1))))

  table <- trajectory_table(records)
  expect_identical(table$decision_point_id, c("a", "b", "c", "d"))
  expect_identical(
    table$group_activation_id,
    rep("group_activation_1", 4L)
  )
  expect_false("decision_plan_metadata" %in% names(table))
  expect_false("post_dispatch_review" %in% table$decision_point_id)
})

test_that("zero-eligible grouped activation emits only opted-in veto leaves", {
  calls <- 0L
  leaves <- list(
    DecisionPoint(
      "audited", NULL,
      condition = function(entity) FALSE,
      audit = TRUE
    ),
    DecisionPoint(
      "quiet", NULL,
      condition = function(entity) FALSE,
      audit = FALSE
    )
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("audited", "quiet"))
  schema <- .s3c_schema(leaves, list(group))
  engine <- load_model(
    schema,
    .s3c_bundle(),
    policy = list(propose_plan = function(...) {
      calls <<- calls + 1L
      stop("must not be called")
    }),
    trajectory = list(detail = "none")
  )

  out <- engine$run(.s3c_entity(schema), max_events = 1)
  records <- out$trajectory_records

  expect_identical(calls, 0L)
  expect_identical(length(records), 1L)
  expect_identical(records[[1L]]$decision_point_id, "audited")
  expect_false(records[[1L]]$condition_met)
  expect_identical(records[[1L]]$grouped_decision_point_id, "joint")
  expect_identical(records[[1L]]$group_activation_id, "group_activation_1")
  expect_null(records[[1L]]$decision_plan_metadata)
})

test_that("activation ids count zero-row firings and reset reproducibly per run", {
  leaves <- list(
    DecisionPoint(
      "late", NULL,
      condition = function(entity) entity$current$checks >= 2L,
      audit = FALSE
    ),
    DecisionPoint(
      "never", NULL,
      condition = function(entity) FALSE,
      audit = FALSE
    )
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("late", "never"))
  schema <- .s3c_schema(leaves, list(group))
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      .s3c_null_plan(
        eligible_decision_points,
        metadata = list(stochastic_score = stats::runif(1))
      )
    }
  )
  engine <- load_model(
    schema,
    .s3c_bundle(n_checks = 2L),
    policy = policy,
    trajectory = list(detail = "none"),
    runtime = RuntimeContext(seed = 222L)
  )

  first <- engine$run(.s3c_entity(schema), max_events = 2)
  second <- engine$run(.s3c_entity(schema), max_events = 2)

  expect_identical(length(first$trajectory_records), 1L)
  expect_identical(length(second$trajectory_records), 1L)
  expect_identical(
    first$trajectory_records[[1L]]$group_activation_id,
    "group_activation_2"
  )
  expect_identical(
    second$trajectory_records[[1L]]$group_activation_id,
    "group_activation_2"
  )
  expect_identical(
    first$trajectory_records[[1L]]$decision_plan_metadata,
    second$trajectory_records[[1L]]$decision_plan_metadata
  )
})

test_that("ordinary trajectory records retain NULL grouped identity", {
  ordinary <- DecisionPoint("ordinary", "CHECK")
  schema <- .s3c_schema(list(ordinary))
  out <- load_model(
    schema,
    .s3c_bundle(),
    trajectory = list(detail = "full")
  )$run(.s3c_entity(schema), max_events = 1)

  expect_identical(length(out$trajectory_records), 1L)
  record <- out$trajectory_records[[1L]]
  expect_identical(record$decision_point_id, "ordinary")
  expect_null(record$grouped_decision_point_id)
  expect_null(record$group_activation_id)
  expect_null(record$decision_plan_metadata)

  table <- trajectory_table(out$trajectory_records)
  expect_true(is.na(table$grouped_decision_point_id[[1L]]))
  expect_true(is.na(table$group_activation_id[[1L]]))
})

test_that("earlier group audit work precedes a later independent group failure", {
  observations <- character()
  observed <- function(id) {
    force(id)
    function(entity) {
      observations <<- c(observations, id)
      list(checks = entity$current$checks)
    }
  }
  leaves <- list(
    DecisionPoint("g1_a", NULL, observation_fn = observed("g1_a")),
    DecisionPoint("g1_b", NULL, condition = function(entity) FALSE),
    DecisionPoint("g2_a", NULL, observation_fn = observed("g2_a")),
    DecisionPoint("g2_b", NULL, condition = function(entity) FALSE)
  )
  groups <- list(
    GroupedDecisionPoint("g1", "CHECK", c("g1_a", "g1_b")),
    GroupedDecisionPoint("g2", "CHECK", c("g2_a", "g2_b"))
  )
  schema <- .s3c_schema(leaves, groups)
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      if (identical(grouped_decision_point$id, "g2")) {
        stop("later group failed")
      }
      .s3c_null_plan(eligible_decision_points)
    }
  )
  engine <- load_model(
    schema,
    .s3c_bundle(),
    policy = policy,
    trajectory = list(detail = "none")
  )

  expect_error(
    engine$run(.s3c_entity(schema), max_events = 1),
    "g2.*later group failed"
  )
  expect_identical(observations, "g1_a")
})

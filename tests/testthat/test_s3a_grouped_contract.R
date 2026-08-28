.s3a_bundle <- function() {
  list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = c("CHECK", "DIRECT", "ACT"),
    propose_events = function(entity) list(),
    transition = function(entity, event) list(),
    stop = function(entity, event) FALSE
  )
}

.s3a_leaves <- function() {
  list(
    DecisionPoint(
      id = "dispatch_response",
      trigger = NULL,
      allowed_actions = c("accept", "decline")
    ),
    DecisionPoint(
      id = "battery_safety",
      trigger = "DIRECT",
      allowed_actions = "return"
    ),
    DecisionPoint(
      id = "route_review",
      trigger = "ROUTE",
      allowed_actions = "reroute"
    )
  )
}

.s3a_group <- function(id = "post_dispatch_review",
                       members = c("dispatch_response", "battery_safety"),
                       trigger = "CHECK") {
  GroupedDecisionPoint(id = id, trigger = trigger, members = members)
}

.s3a_schema <- function(leaves = .s3a_leaves(), groups = list(.s3a_group())) {
  set_schema(
    vars = list(x = list(type = "nonnegative_integer", default = 0L)),
    time_spec = time_spec(unit = "hours"),
    decision_points = leaves,
    decision_groups = groups
  )
}


# DecisionPoint group-only trigger ------------------------------------------

test_that("DecisionPoint distinguishes an omitted trigger from explicit NULL", {
  expect_error(
    DecisionPoint(id = "group_only"),
    "`trigger` must be supplied",
    fixed = TRUE
  )

  leaf <- DecisionPoint("group_only", NULL, "act", NULL, NULL, TRUE, NULL, "Grouped leaf")
  expect_s3_class(leaf, "DecisionPoint")
  expect_null(leaf$trigger)
  expect_identical(leaf$allowed_actions, "act")
  expect_true(leaf$audit)
  expect_identical(leaf$label, "Grouped leaf")
  expect_false(dp_fires(leaf, list(event_type = "anything")))
  expect_output(print(leaf), "group-only; no direct trigger", fixed = TRUE)
})


# GroupedDecisionPoint constructor -----------------------------------------

test_that("GroupedDecisionPoint preserves trigger and member declaration order", {
  group <- GroupedDecisionPoint(
    "joint_review",
    c("CHECK", "RECHECK"),
    c("battery_safety", "dispatch_response"),
    "Joint dispatch review"
  )

  expect_s3_class(group, "GroupedDecisionPoint")
  expect_identical(group$id, "joint_review")
  expect_identical(group$trigger, c("CHECK", "RECHECK"))
  expect_identical(group$members, c("battery_safety", "dispatch_response"))
  expect_identical(group$label, "Joint dispatch review")
  expect_output(print(group), "battery_safety, dispatch_response", fixed = TRUE)

  predicate <- function(event) identical(event$event_type, "CHECK")
  predicate_group <- GroupedDecisionPoint("predicate", predicate, c("a", "b"))
  expect_identical(predicate_group$trigger, predicate)
  expect_output(print(predicate_group), "predicate function", fixed = TRUE)
})

test_that("GroupedDecisionPoint rejects malformed local declarations", {
  expect_error(GroupedDecisionPoint("", "CHECK", c("a", "b")), "`id`")
  expect_error(GroupedDecisionPoint(NA_character_, "CHECK", c("a", "b")), "`id`")
  expect_error(GroupedDecisionPoint(id = "g", members = c("a", "b")), "`trigger` must be supplied")
  expect_error(GroupedDecisionPoint("g", NULL, c("a", "b")), "cannot be NULL", fixed = TRUE)
  expect_error(GroupedDecisionPoint("g", character(), c("a", "b")), "`trigger`")
  expect_error(GroupedDecisionPoint("g", c("CHECK", NA_character_), c("a", "b")), "`trigger`")
  expect_error(GroupedDecisionPoint("g", "CHECK"), "`members`")
  expect_error(GroupedDecisionPoint("g", "CHECK", "a"), "at least two", fixed = TRUE)
  expect_error(GroupedDecisionPoint("g", "CHECK", c("a", "")), "non-empty")
  expect_error(GroupedDecisionPoint("g", "CHECK", c("a", "a")), "distinct")
  expect_error(GroupedDecisionPoint("g", "CHECK", c("a", "b"), label = NA_character_), "`label`")
})


# DecisionPlan constructor --------------------------------------------------

test_that("DecisionPlan retains named ActionEvent and explicit NULL selections", {
  selections <- list(
    battery_safety = ActionEvent("return", time_next = 2),
    dispatch_response = NULL
  )
  plan <- DecisionPlan(
    selections,
    metadata = list(strategy = "battery_first", score = 0.91)
  )

  expect_s3_class(plan, "DecisionPlan")
  expect_identical(names(plan$selections), c("battery_safety", "dispatch_response"))
  expect_s3_class(plan$selections$battery_safety, "ActionEvent")
  expect_null(plan$selections$dispatch_response)
  expect_identical(plan$metadata$strategy, "battery_first")
  expect_output(print(plan), "dispatch_response : (no new action)", fixed = TRUE)
  expect_output(print(plan), "metadata  : strategy, score", fixed = TRUE)

  empty_metadata <- DecisionPlan(list(dispatch_response = NULL), metadata = list())
  expect_identical(empty_metadata$metadata, list())
})

test_that("DecisionPlan rejects malformed local selections and metadata", {
  expect_error(DecisionPlan(), "`selections`")
  expect_error(DecisionPlan(list()), "non-empty")
  expect_error(DecisionPlan(list(ActionEvent("act", 1))), "non-empty name")
  expect_error(DecisionPlan(structure(list(NULL), names = "")), "non-empty name")
  expect_error(DecisionPlan(structure(list(NULL), names = NA_character_)), "non-empty name")
  expect_error(
    DecisionPlan(structure(list(NULL, NULL), names = c("a", "a"))),
    "unique"
  )
  expect_error(DecisionPlan(list(a = "not an action")), "ActionEvent or explicit NULL")
  expect_error(DecisionPlan(list(a = NULL), metadata = "opaque"), "named list or NULL")
  expect_error(DecisionPlan(list(a = NULL), metadata = list("unnamed")), "non-empty name")
  expect_error(
    DecisionPlan(
      list(a = NULL),
      metadata = structure(list(1, 2), names = c("score", "score"))
    ),
    "unique"
  )
})


# Schema contract -----------------------------------------------------------

test_that("set_schema stores leaves and groups separately in declaration order", {
  leaves <- .s3a_leaves()
  groups <- list(
    .s3a_group("second", c("battery_safety", "dispatch_response")),
    .s3a_group("first", c("dispatch_response", "route_review"), "RECHECK")
  )
  schema <- .s3a_schema(leaves, groups)

  expect_identical(
    vapply(schema$decision_points, `[[`, character(1), "id"),
    c("dispatch_response", "battery_safety", "route_review")
  )
  expect_identical(
    vapply(schema$decision_groups, `[[`, character(1), "id"),
    c("second", "first")
  )
  expect_identical(
    schema$decision_groups[[1]]$members,
    c("battery_safety", "dispatch_response")
  )
})

test_that("set_schema supports direct-only, group-only, and dual-use leaves", {
  direct_only <- set_schema(
    vars = list(x = "count"),
    time_spec = time_spec(unit = "hours"),
    decision_points = list(DecisionPoint("direct", "DIRECT"))
  )
  expect_null(direct_only$decision_groups)

  grouped <- .s3a_schema()
  expect_null(grouped$decision_points[[1]]$trigger)
  expect_identical(grouped$decision_points[[2]]$trigger, "DIRECT")

  # A normal leaf may also be a member. Even a shared raw trigger is a valid
  # declaration; overlap is detected from actual fired triggers by the Engine.
  potentially_overlapping <- set_schema(
    vars = list(x = "count"),
    time_spec = time_spec(unit = "hours"),
    decision_points = list(
      DecisionPoint("dual", "CHECK"),
      DecisionPoint("group_only", NULL)
    ),
    decision_groups = list(
      GroupedDecisionPoint("review", "CHECK", c("dual", "group_only"))
    )
  )
  expect_identical(potentially_overlapping$decision_points[[1]]$trigger, "CHECK")
})

test_that("full no-group schemas expose NULL decision_groups and variables-only schemas do not change", {
  full <- set_schema(
    vars = list(x = "count"),
    time_spec = time_spec(unit = "hours")
  )
  expect_identical(
    names(full),
    c("variables", "time_spec", "decision_points", "decision_groups")
  )
  expect_null(full$decision_points)
  expect_null(full$decision_groups)

  variables_only <- set_schema(vars = list(x = "count"))
  expect_identical(names(variables_only), "x")
  expect_null(variables_only$decision_groups)
})

test_that("set_schema rejects invalid grouped cross-references", {
  leaves <- .s3a_leaves()
  ts <- time_spec(unit = "hours")

  expect_error(
    set_schema(vars = list(x = "count"), decision_groups = list(.s3a_group())),
    "`time_spec` is required"
  )
  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = leaves, decision_groups = list("not a group")
    ),
    "GroupedDecisionPoint"
  )
  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = c(leaves, list(DecisionPoint("battery_safety", "X"))),
      decision_groups = list(.s3a_group())
    ),
    "duplicated DecisionPoint id"
  )
  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = leaves,
      decision_groups = list(.s3a_group("same"), .s3a_group("same"))
    ),
    "duplicated GroupedDecisionPoint id"
  )
  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = leaves,
      decision_groups = list(.s3a_group("battery_safety"))
    ),
    "globally unique"
  )
  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = leaves,
      decision_groups = list(.s3a_group(members = c("dispatch_response", "missing")))
    ),
    "unknown DecisionPoint member id"
  )

  nested_groups <- list(
    .s3a_group("first_group"),
    .s3a_group("second_group", c("route_review", "first_group"))
  )
  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = leaves, decision_groups = nested_groups
    ),
    "nested groups are not supported"
  )

  expect_error(
    set_schema(
      vars = list(x = "count"), time_spec = ts,
      decision_points = list(DecisionPoint("unreferenced", NULL))
    ),
    "must be referenced by at least one GroupedDecisionPoint"
  )
})


# Defensive load_model validation and serialization ------------------------

test_that("load_model defensively repeats essential grouped schema checks", {
  schema <- .s3a_schema()
  grouped_policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      selections <- rep(list(NULL), length(eligible_decision_points))
      names(selections) <- names(eligible_decision_points)
      DecisionPlan(selections)
    }
  )
  engine <- load_model(schema, .s3a_bundle(), policy = grouped_policy)
  expect_s3_class(engine, "Engine")
  expect_identical(engine$.schema$decision_groups, schema$decision_groups)

  unknown <- schema
  unknown$decision_groups[[1]]$members <- c("dispatch_response", "unknown")
  expect_error(load_model(unknown, .s3a_bundle()), "unknown DecisionPoint member id")

  unreferenced <- schema
  unreferenced$decision_groups <- NULL
  expect_error(
    load_model(unreferenced, .s3a_bundle()),
    "must be referenced by at least one GroupedDecisionPoint"
  )

  invalid_id <- schema
  invalid_id$decision_groups[[1]]$id <- NA_character_
  invalid_id$decision_groups[[1]]$allowed_actions <- "not allowed on a group"
  expect_error(load_model(invalid_id, .s3a_bundle()), "decision_groups\\[\\[1\\]\\]\\$id")

  missing_id <- schema
  missing_id$decision_groups[[1]]$id <- NULL
  missing_id$decision_groups[[1]]$allowed_actions <- "not allowed on a group"
  expect_error(load_model(missing_id, .s3a_bundle()), "exactly one `id`")

  leaf_field <- schema
  leaf_field$decision_groups[[1]]$allowed_actions <- "not allowed on a group"
  expect_error(load_model(leaf_field, .s3a_bundle()), "leaf-only field")
})

test_that("group declarations and plans survive base serialization without reordering", {
  schema <- .s3a_schema()
  plan <- DecisionPlan(
    list(dispatch_response = NULL, battery_safety = ActionEvent("return", 3)),
    metadata = list(strategy = "battery_first")
  )

  restored_schema <- unserialize(serialize(schema, NULL))
  restored_plan <- unserialize(serialize(plan, NULL))

  expect_s3_class(restored_schema$decision_groups[[1]], "GroupedDecisionPoint")
  expect_identical(
    restored_schema$decision_groups[[1]]$members,
    c("dispatch_response", "battery_safety")
  )
  expect_s3_class(restored_plan, "DecisionPlan")
  expect_identical(
    names(restored_plan$selections),
    c("dispatch_response", "battery_safety")
  )
  expect_null(restored_plan$selections$dispatch_response)
  expect_identical(restored_plan$metadata$strategy, "battery_first")
})

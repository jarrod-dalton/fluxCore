.s3b_vars <- function() {
  list(
    checks = list(type = "nonnegative_integer", default = 0L),
    x = list(type = "nonnegative_integer", default = 0L)
  )
}

.s3b_schema <- function(decision_points, decision_groups = NULL) {
  set_schema(
    vars = .s3b_vars(),
    time_spec = time_spec(unit = "hours"),
    decision_points = decision_points,
    decision_groups = decision_groups
  )
}

.s3b_entity <- function(schema, id = "courier_1") {
  Entity$new(schema = schema$variables, id = id)
}

.s3b_bundle <- function(n_checks = 1L,
                        event_catalog = c("CHECK", "DIRECT", "A", "B"),
                        transition_hook = NULL,
                        stop_hook = NULL) {
  list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = event_catalog,
    propose_events = function(entity) {
      n <- entity$current$checks
      if (n >= n_checks) return(list())
      list(check = list(time_next = as.numeric(n + 1L), event_type = "CHECK"))
    },
    transition = function(entity, event) {
      if (!is.null(transition_hook)) transition_hook(entity, event)
      if (identical(event$event_type, "CHECK")) {
        list(
          checks = entity$current$checks + 1L,
          x = entity$current$x + 1L
        )
      } else {
        list()
      }
    },
    stop = function(entity, event) {
      if (is.null(stop_hook)) FALSE else isTRUE(stop_hook(entity, event))
    }
  )
}

.s3b_null_plan <- function(eligible_decision_points) {
  selections <- rep(list(NULL), length(eligible_decision_points))
  names(selections) <- names(eligible_decision_points)
  DecisionPlan(selections)
}

.s3b_basic_declarations <- function(condition_a = NULL, condition_b = NULL,
                                    trigger_a = NULL) {
  leaves <- list(
    DecisionPoint(
      "a", trigger_a,
      allowed_actions = c("A", "A_OLD", "A_NEW"),
      condition = condition_a
    ),
    DecisionPoint(
      "b", NULL,
      allowed_actions = c("B", "B_OLD", "B_NEW"),
      condition = condition_b
    )
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  list(leaves = leaves, group = group)
}


# Load boundary and basic consultation -------------------------------------

test_that("load_model requires propose_plan only for non-empty decision groups", {
  declarations <- .s3b_basic_declarations()
  grouped_schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  bundle <- .s3b_bundle()

  expect_error(load_model(grouped_schema, bundle), "requires.*propose_plan")
  expect_error(
    load_model(grouped_schema, bundle, policy = function(...) NULL),
    "requires.*propose_plan"
  )
  expect_error(
    load_model(
      grouped_schema, bundle,
      policy = list(propose_action = function(...) NULL)
    ),
    "requires.*propose_plan"
  )

  no_group_schema <- .s3b_schema(
    list(DecisionPoint("ordinary", "CHECK", allowed_actions = "A"))
  )
  expect_s3_class(
    load_model(no_group_schema, bundle, policy = function(...) NULL),
    "Engine"
  )
})

test_that("group policy receives one named declared-order eligible list and typed contexts", {
  seen <- new.env(parent = emptyenv())
  seen$calls <- 0L
  seen$ids <- NULL
  seen$group <- NULL
  seen$sim_ctx <- NULL
  seen$param_ctx <- NULL
  seen$realized_decision_point_id <- NULL

  declarations <- .s3b_basic_declarations(
    condition_a = function(entity) entity$current$x == 1L,
    condition_b = function(entity) FALSE,
    trigger_a = "NEVER"
  )
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity,
                            sim_ctx,
                            param_ctx) {
      seen$calls <- seen$calls + 1L
      seen$ids <- names(eligible_decision_points)
      seen$group <- grouped_decision_point
      seen$sim_ctx <- sim_ctx
      seen$param_ctx <- param_ctx
      DecisionPlan(list(a = ActionEvent("A", entity$last_time + 1)))
    }
  )

  bundle <- .s3b_bundle(
    transition_hook = function(entity, event) {
      if (identical(event$event_type, "A")) {
        seen$realized_decision_point_id <- event$decision_point_id
      }
    }
  )
  out <- load_model(schema, bundle, policy = policy)$run(
    .s3b_entity(schema), max_events = 5
  )

  expect_identical(seen$calls, 1L)
  expect_identical(seen$ids, "a")
  expect_identical(seen$group$id, "joint")
  expect_s3_class(seen$sim_ctx, "SimContext")
  expect_s3_class(seen$param_ctx, "ParamContext")
  expect_identical(seen$realized_decision_point_id, "a")
  expect_identical(out$events$event_type, c("init", "CHECK", "A"))
  expect_identical(out$events$time, c(0, 1, 2))
})

test_that("a zero-eligible group skips propose_plan after the one transition", {
  calls <- 0L
  transitions <- 0L
  declarations <- .s3b_basic_declarations(
    condition_a = function(entity) FALSE,
    condition_b = function(entity) FALSE
  )
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  policy <- list(
    propose_plan = function(...) {
      calls <<- calls + 1L
      stop("must not be called")
    }
  )
  bundle <- .s3b_bundle(
    transition_hook = function(entity, event) transitions <<- transitions + 1L
  )

  out <- load_model(schema, bundle, policy = policy)$run(.s3b_entity(schema))
  expect_identical(calls, 0L)
  expect_identical(transitions, 1L)
  expect_identical(out$entity$current$x, 1L)
  expect_identical(out$events$event_type, c("init", "CHECK"))
})

test_that("runtime policy tampering remains a strict grouped error", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  engine <- load_model(
    schema, .s3b_bundle(),
    policy = list(propose_plan = function(g, e, entity) .s3b_null_plan(e))
  )
  engine$.policy <- NULL
  expect_error(engine$run(.s3b_entity(schema)), "requires policy\\$propose_plan")
})


# Activation timing, condition freeze, and deterministic order -------------

test_that("overlap is rejected before transition, Entity mutation, conditions, or policy", {
  transition_calls <- 0L
  condition_calls <- 0L
  policy_calls <- 0L
  leaves <- list(
    DecisionPoint(
      "dual", "CHECK",
      condition = function(entity) {
        condition_calls <<- condition_calls + 1L
        TRUE
      }
    ),
    DecisionPoint("group_only", NULL)
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("dual", "group_only"))
  schema <- .s3b_schema(leaves, list(group))
  entity <- .s3b_entity(schema)
  bundle <- .s3b_bundle(
    transition_hook = function(entity, event) transition_calls <<- transition_calls + 1L
  )
  policy <- list(
    propose_action = function(...) {
      policy_calls <<- policy_calls + 1L
      NULL
    },
    propose_plan = function(...) {
      policy_calls <<- policy_calls + 1L
      stop("must not be called")
    }
  )

  expect_error(
    load_model(schema, bundle, policy = policy)$run(entity),
    "ambiguous decision activation.*dual"
  )
  expect_identical(transition_calls, 0L)
  expect_identical(condition_calls, 0L)
  expect_identical(policy_calls, 0L)
  expect_identical(entity$current$x, 0L)
  expect_identical(entity$last_time, 0)
  expect_identical(entity$events$event_type, "init")
})

test_that("two fired groups sharing a leaf fail at the same pre-transition gate", {
  transition_calls <- 0L
  leaves <- list(
    DecisionPoint("shared", NULL),
    DecisionPoint("left", NULL),
    DecisionPoint("right", NULL)
  )
  groups <- list(
    GroupedDecisionPoint("g1", "CHECK", c("shared", "left")),
    GroupedDecisionPoint("g2", "CHECK", c("shared", "right"))
  )
  schema <- .s3b_schema(leaves, groups)
  entity <- .s3b_entity(schema)
  policy <- list(propose_plan = function(...) stop("must not be called"))

  expect_error(
    load_model(
      schema,
      .s3b_bundle(
        transition_hook = function(entity, event) transition_calls <<- transition_calls + 1L
      ),
      policy = policy
    )$run(entity),
    "ambiguous decision activation.*shared"
  )
  expect_identical(transition_calls, 0L)
  expect_identical(entity$events$event_type, "init")
})

test_that("eligibility freezes ordinary-then-group before ordinary-first policy dispatch", {
  condition_order <- character()
  policy_order <- character()
  ordinary_policy_ran <- FALSE
  transition_calls <- 0L

  condition <- function(id, result = TRUE) {
    force(id)
    force(result)
    function(entity) {
      condition_order <<- c(condition_order, id)
      expect_identical(entity$current$x, 1L)
      if (identical(id, "g1a")) return(!ordinary_policy_ran)
      result
    }
  }
  leaves <- list(
    DecisionPoint("o1", "CHECK", condition = condition("o1")),
    DecisionPoint("g1a", NULL, condition = condition("g1a")),
    DecisionPoint("o2", "CHECK", condition = condition("o2")),
    DecisionPoint("g1b", NULL, condition = condition("g1b")),
    DecisionPoint("g2a", NULL, condition = condition("g2a")),
    DecisionPoint("g2b", NULL, condition = condition("g2b"))
  )
  groups <- list(
    GroupedDecisionPoint("g1", "CHECK", c("g1a", "g1b")),
    GroupedDecisionPoint("g2", "CHECK", c("g2a", "g2b"))
  )
  schema <- .s3b_schema(leaves, groups)
  policy <- list(
    propose_action = function(decision_point, entity) {
      policy_order <<- c(policy_order, paste0("ordinary:", decision_point$id))
      ordinary_policy_ran <<- TRUE
      NULL
    },
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      policy_order <<- c(policy_order, paste0("group:", grouped_decision_point$id))
      if (identical(grouped_decision_point$id, "g1")) {
        expect_identical(names(eligible_decision_points), c("g1a", "g1b"))
      }
      .s3b_null_plan(eligible_decision_points)
    }
  )
  bundle <- .s3b_bundle(
    transition_hook = function(entity, event) transition_calls <<- transition_calls + 1L
  )

  load_model(schema, bundle, policy = policy)$run(.s3b_entity(schema))

  expect_identical(transition_calls, 1L)
  expect_identical(
    condition_order,
    c("o1", "o2", "g1a", "g1b", "g2a", "g2b")
  )
  expect_identical(
    policy_order,
    c("ordinary:o1", "ordinary:o2", "group:g1", "group:g2")
  )
})

test_that("an action handler applies once before its action event fires a group", {
  transition_calls <- 0L
  handler_calls <- 0L
  group_calls <- character()
  leaves <- list(
    DecisionPoint(
      "scheduler", NULL,
      allowed_actions = "A",
      action_handlers = list(
        A = function(entity, event) {
          handler_calls <<- handler_calls + 1L
          list(x = entity$current$x + 1L)
        }
      )
    ),
    DecisionPoint("schedule_peer", NULL),
    DecisionPoint("after_a", NULL),
    DecisionPoint("after_a_peer", NULL)
  )
  groups <- list(
    GroupedDecisionPoint("schedule", "CHECK", c("scheduler", "schedule_peer")),
    GroupedDecisionPoint("after_action", "A", c("after_a", "after_a_peer"))
  )
  schema <- .s3b_schema(leaves, groups)
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      group_calls <<- c(group_calls, grouped_decision_point$id)
      if (identical(grouped_decision_point$id, "schedule")) {
        return(DecisionPlan(list(
          scheduler = ActionEvent("A", entity$last_time + 1),
          schedule_peer = NULL
        )))
      }
      .s3b_null_plan(eligible_decision_points)
    }
  )
  bundle <- .s3b_bundle(
    event_catalog = "CHECK",
    transition_hook = function(entity, event) {
      transition_calls <<- transition_calls + 1L
    }
  )

  out <- load_model(schema, bundle, policy = policy)$run(.s3b_entity(schema))

  expect_identical(transition_calls, 1L)
  expect_identical(handler_calls, 1L)
  expect_identical(group_calls, c("schedule", "after_action"))
  expect_identical(out$entity$current$x, 2L)
  expect_identical(out$events$event_type, c("init", "CHECK", "A"))
})

test_that("group condition errors are fail-fast after the committed transition", {
  policy_calls <- 0L
  leaves <- list(
    DecisionPoint("bad", NULL, condition = function(entity) stop("condition boom")),
    DecisionPoint("other", NULL)
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("bad", "other"))
  schema <- .s3b_schema(leaves, list(group))
  entity <- .s3b_entity(schema)
  policy <- list(
    propose_plan = function(...) {
      policy_calls <<- policy_calls + 1L
      stop("must not be called")
    }
  )

  expect_error(
    load_model(schema, .s3b_bundle(), policy = policy)$run(entity),
    "DecisionPoint\\('bad'\\).*condition boom"
  )
  expect_identical(policy_calls, 0L)
  expect_identical(entity$current$x, 1L)
  expect_identical(entity$last_time, 1)
  expect_identical(entity$events$event_type, c("init", "CHECK"))
})


# Strict DecisionPlan and ActionEvent validation ----------------------------

test_that("grouped dispatch rejects malformed and incomplete plans strictly", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  bundle <- .s3b_bundle()

  duplicate_plan <- structure(
    list(
      selections = structure(list(NULL, NULL), names = c("a", "a")),
      metadata = NULL
    ),
    class = "DecisionPlan"
  )
  invalid_selection <- structure(
    list(selections = list(a = "bad", b = NULL), metadata = NULL),
    class = "DecisionPlan"
  )
  invalid_metadata <- structure(
    list(selections = list(a = NULL, b = NULL), metadata = "bad"),
    class = "DecisionPlan"
  )
  spoofed_action <- structure(1, class = "ActionEvent")
  cases <- list(
    bare_null = list(result = NULL, message = "must be a DecisionPlan"),
    wrong_class = list(result = list(a = NULL, b = NULL), message = "must be a DecisionPlan"),
    missing = list(result = DecisionPlan(list(a = NULL)), message = "missing \\{b\\}"),
    extra = list(
      result = DecisionPlan(list(a = NULL, b = NULL, typo = NULL)),
      message = "extra \\{typo\\}"
    ),
    duplicate = list(result = duplicate_plan, message = "unique, non-empty names"),
    invalid_selection = list(result = invalid_selection, message = "DecisionPoint\\('a'\\).*ActionEvent"),
    invalid_metadata = list(result = invalid_metadata, message = "metadata"),
    spoofed_action = list(
      result = DecisionPlan(list(a = spoofed_action, b = NULL)),
      message = "DecisionPoint\\('a'\\).*ActionEvent"
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    policy <- list(propose_plan = local({
      result <- case$result
      function(...) result
    }))
    expect_error(
      load_model(schema, bundle, policy = policy)$run(.s3b_entity(schema)),
      case$message,
      info = case_name
    )
  }
})

test_that("propose_plan callback errors retain grouped context", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  expect_error(
    load_model(
      schema,
      .s3b_bundle(),
      policy = list(propose_plan = function(...) stop("joint policy boom"))
    )$run(.s3b_entity(schema)),
    "policy\\$propose_plan.*GroupedDecisionPoint\\('joint'\\).*joint policy boom"
  )
})

test_that("grouped actions enforce member provenance, allowed actions, and event data", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))

  event_mismatch <- ActionEvent("A", 2)
  event_mismatch$event_type <- "B"
  bad_params <- ActionEvent("A", 2)
  bad_params$params <- "not a list"
  missing_params <- ActionEvent("A", 2)
  missing_params["params"] <- NULL
  duplicate_time <- c(unclass(ActionEvent("A", 2)), list(time_next = 3))
  class(duplicate_time) <- "ActionEvent"
  malformed_provenance <- ActionEvent("A", 2)
  malformed_provenance$decision_point_id <- character()
  cases <- list(
    disallowed = list(
      action = ActionEvent("B", 2),
      bundle = .s3b_bundle(),
      message = "joint.*DecisionPoint\\('a'\\).*not in allowed_actions"
    ),
    provenance = list(
      action = ActionEvent("A", 2, decision_point_id = "b"),
      bundle = .s3b_bundle(),
      message = "decision_point_id 'b'.*'a'"
    ),
    event_mismatch = list(
      action = event_mismatch,
      bundle = .s3b_bundle(),
      message = "event_type.*identical"
    ),
    past_time = list(
      action = ActionEvent("A", 0.5),
      bundle = .s3b_bundle(),
      message = "before the current model time"
    ),
    params = list(
      action = bad_params,
      bundle = .s3b_bundle(),
      message = "invalid `params`"
    ),
    missing_required_field = list(
      action = missing_params,
      bundle = .s3b_bundle(),
      message = "malformed ActionEvent"
    ),
    duplicate_required_field = list(
      action = duplicate_time,
      bundle = .s3b_bundle(),
      message = "malformed ActionEvent"
    ),
    malformed_provenance = list(
      action = malformed_provenance,
      bundle = .s3b_bundle(),
      message = "invalid `decision_point_id`"
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    policy <- list(propose_plan = local({
      action <- case$action
      function(...) DecisionPlan(list(a = action, b = NULL))
    }))
    expect_error(
      load_model(schema, case$bundle, policy = policy)$run(.s3b_entity(schema)),
      case$message,
      info = case_name
    )
  }

  uncatalogued_leaves <- list(
    DecisionPoint("a", NULL, allowed_actions = "UNCATALOGUED"),
    DecisionPoint("b", NULL)
  )
  uncatalogued_schema <- .s3b_schema(
    uncatalogued_leaves,
    list(GroupedDecisionPoint("joint", "CHECK", c("a", "b")))
  )
  expect_error(
    load_model(
      uncatalogued_schema,
      .s3b_bundle(event_catalog = "CHECK"),
      policy = list(
        propose_plan = function(...) {
          DecisionPlan(list(a = ActionEvent("UNCATALOGUED", 2), b = NULL))
        }
      )
    )$run(.s3b_entity(uncatalogued_schema)),
    "not declared in the model event catalog"
  )
})

test_that("plan normalization uses canonical member order and preserves opaque metadata", {
  leaves <- list(
    DecisionPoint("a", NULL, allowed_actions = "A"),
    DecisionPoint("b", NULL, allowed_actions = "B")
  )
  names(leaves) <- c("a", "b")
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  metadata <- list(
    strategy = "reverse_return_order",
    diagnostics = list(score = 0.75)
  )
  reversed <- DecisionPlan(
    list(
      b = ActionEvent("B", 3, decision_point_id = "b"),
      a = ActionEvent("A", 2)
    ),
    metadata = metadata
  )

  normalized <- fluxCore:::.validate_group_decision_plan(
    reversed,
    group = group,
    eligible_decision_points = leaves,
    current_time = 1,
    event_catalog = c("A", "B")
  )
  expect_identical(names(normalized$selections), c("a", "b"))
  expect_identical(normalized$selections$a$decision_point_id, "a")
  expect_identical(normalized$selections$b$decision_point_id, "b")
  expect_identical(normalized$metadata, metadata)

  preflight <- fluxCore:::.preflight_group_pending_actions(
    normalized,
    group = group,
    eligible_decision_points = leaves,
    pending_actions = list()
  )
  expect_identical(names(preflight$candidate), c("a", "b"))
})

test_that("opaque plan metadata cannot change grouped staging or realized events", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  run_with_metadata <- function(metadata) {
    policy <- list(
      propose_plan = function(...) {
        DecisionPlan(
          list(a = ActionEvent("A", 2), b = NULL),
          metadata = metadata
        )
      }
    )
    load_model(schema, .s3b_bundle(), policy = policy)$run(.s3b_entity(schema))
  }

  without_metadata <- run_with_metadata(NULL)
  with_metadata <- run_with_metadata(list(strategy = "audit_only", score = 99))
  expect_identical(with_metadata$events, without_metadata$events)
})

test_that("allowed catalog actions and auto-registered handler actions both remain valid", {
  # No handler: allowed action is valid when declared in the bundle catalog.
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  policy <- list(
    propose_plan = function(...) {
      DecisionPlan(list(a = ActionEvent("A", 2), b = NULL))
    }
  )
  out <- load_model(schema, .s3b_bundle(), policy = policy)$run(.s3b_entity(schema))
  expect_true("A" %in% out$events$event_type)

  # Handler: its action type is auto-registered even when absent from the
  # bundle's explicit event_catalog, matching the ordinary action contract.
  handler_leaves <- list(
    DecisionPoint(
      "a", NULL,
      allowed_actions = "HANDLED",
      action_handlers = list(HANDLED = function(entity, event) list())
    ),
    DecisionPoint("b", NULL)
  )
  handler_schema <- .s3b_schema(
    handler_leaves,
    list(GroupedDecisionPoint("joint", "CHECK", c("a", "b")))
  )
  handler_out <- load_model(
    handler_schema,
    .s3b_bundle(event_catalog = "CHECK"),
    policy = list(
      propose_plan = function(...) {
        DecisionPlan(list(a = ActionEvent("HANDLED", 2), b = NULL))
      }
    )
  )$run(.s3b_entity(handler_schema))
  expect_true("HANDLED" %in% handler_out$events$event_type)
})


# Pending-store preflight and observable outcomes --------------------------

.s3b_pending_case <- function(mode_a = "replace",
                              mode_b = "replace",
                              second_a = c("action", "null"),
                              second_b = c("null", "action")) {
  second_a <- match.arg(second_a)
  second_b <- match.arg(second_b)
  leaves <- list(
    DecisionPoint(
      "a", NULL,
      allowed_actions = c("A_OLD", "A_NEW"),
      on_pending_action = mode_a
    ),
    DecisionPoint(
      "b", NULL,
      allowed_actions = c("B_OLD", "B_NEW"),
      on_pending_action = mode_b
    )
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  schema <- .s3b_schema(leaves, list(group))
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      if (entity$current$checks == 1L) {
        return(DecisionPlan(list(
          a = ActionEvent("A_OLD", 6),
          b = if (identical(second_b, "action")) ActionEvent("B_OLD", 8) else NULL
        )))
      }
      DecisionPlan(list(
        a = if (identical(second_a, "action")) ActionEvent("A_NEW", 7) else NULL,
        b = if (identical(second_b, "action")) ActionEvent("B_NEW", 9) else NULL
      ))
    }
  )
  bundle <- .s3b_bundle(
    n_checks = 2L,
    event_catalog = c("CHECK", "A_OLD", "A_NEW", "B_OLD", "B_NEW")
  )
  list(
    engine = load_model(schema, bundle, policy = policy),
    entity = .s3b_entity(schema),
    schema = schema
  )
}

test_that("grouped NULL and keep preserve pending actions while replace selects new", {
  null_case <- .s3b_pending_case(mode_a = "error", second_a = "null")
  null_out <- null_case$engine$run(null_case$entity, max_events = 10)
  expect_true("A_OLD" %in% null_out$events$event_type)
  expect_false("A_NEW" %in% null_out$events$event_type)

  keep_case <- .s3b_pending_case(mode_a = "keep", second_a = "action")
  keep_out <- keep_case$engine$run(keep_case$entity, max_events = 10)
  expect_true("A_OLD" %in% keep_out$events$event_type)
  expect_false("A_NEW" %in% keep_out$events$event_type)

  replace_case <- .s3b_pending_case(mode_a = "replace", second_a = "action")
  replace_out <- replace_case$engine$run(replace_case$entity, max_events = 10)
  expect_false("A_OLD" %in% replace_out$events$event_type)
  expect_true("A_NEW" %in% replace_out$events$event_type)
})

test_that("grouped warn aggregates replacements once and commits the new actions", {
  case <- .s3b_pending_case(
    mode_a = "warn", mode_b = "warn",
    second_a = "action", second_b = "action"
  )
  warnings <- character()
  out <- withCallingHandlers(
    case$engine$run(case$entity, max_events = 12),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings[[1]], "\\{a, b\\}")
  expect_false(any(c("A_OLD", "B_OLD") %in% out$events$event_type))
  expect_true(all(c("A_NEW", "B_NEW") %in% out$events$event_type))
})

test_that("grouped error and warning-as-error abort before any candidate commit", {
  error_case <- .s3b_pending_case(mode_a = "error", second_a = "action")
  expect_error(
    error_case$engine$run(error_case$entity, max_events = 10),
    "complete plan.*on_pending_action = 'error'"
  )
  expect_identical(
    error_case$entity$events$event_type,
    c("init", "CHECK", "CHECK")
  )

  warn_case <- .s3b_pending_case(mode_a = "warn", second_a = "action")
  old_options <- options(warn = 2)
  on.exit(options(old_options), add = TRUE)
  expect_error(
    warn_case$engine$run(warn_case$entity, max_events = 10),
    "GroupedDecisionPoint.*replaced still-pending"
  )
  expect_identical(
    warn_case$entity$events$event_type,
    c("init", "CHECK", "CHECK")
  )
})

test_that("pure grouped preflight leaves its input unchanged on a later error", {
  leaves <- list(
    DecisionPoint("a", NULL, on_pending_action = "warn"),
    DecisionPoint("b", NULL, on_pending_action = "error")
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  eligible <- leaves
  names(eligible) <- c("a", "b")
  pending <- list(
    a = ActionEvent("A_OLD", 6, decision_point_id = "a"),
    b = ActionEvent("B_OLD", 8, decision_point_id = "b")
  )
  before <- pending
  plan <- DecisionPlan(list(
    a = ActionEvent("A_NEW", 7, decision_point_id = "a"),
    b = ActionEvent("B_NEW", 9, decision_point_id = "b")
  ))

  expect_error(
    fluxCore:::.preflight_group_pending_actions(plan, group, eligible, pending),
    "on_pending_action = 'error'"
  )
  expect_identical(pending, before)
})

test_that("an earlier group candidate remains committed when a later group preflight fails", {
  first_leaves <- list(
    a = DecisionPoint("a", NULL),
    b = DecisionPoint("b", NULL)
  )
  first_group <- GroupedDecisionPoint("first", "CHECK", c("a", "b"))
  first_plan <- DecisionPlan(list(
    a = ActionEvent("A", 5, decision_point_id = "a"),
    b = NULL
  ))
  first <- fluxCore:::.preflight_group_pending_actions(
    first_plan, first_group, first_leaves, pending_actions = list()
  )
  committed <- first$candidate

  second_leaves <- list(
    c = DecisionPoint("c", NULL, on_pending_action = "error"),
    d = DecisionPoint("d", NULL)
  )
  second_group <- GroupedDecisionPoint("second", "CHECK", c("c", "d"))
  second_plan <- DecisionPlan(list(
    c = ActionEvent("C_NEW", 6, decision_point_id = "c"),
    d = NULL
  ))
  committed$c <- ActionEvent("C_OLD", 7, decision_point_id = "c")
  before_second <- committed

  expect_error(
    fluxCore:::.preflight_group_pending_actions(
      second_plan, second_group, second_leaves, committed
    ),
    "GroupedDecisionPoint\\('second'\\).*on_pending_action = 'error'"
  )
  expect_identical(committed, before_second)
  expect_identical(committed$a$action_type, "A")
})

test_that("pure preflight resolves NULL, keep, replace, and warn in member order", {
  leaves <- list(
    DecisionPoint("null_member", NULL, on_pending_action = "error"),
    DecisionPoint("keep_member", NULL, on_pending_action = "keep"),
    DecisionPoint("replace_member", NULL, on_pending_action = "replace"),
    DecisionPoint("warn_member", NULL, on_pending_action = "warn")
  )
  names(leaves) <- vapply(leaves, `[[`, character(1), "id")
  group <- GroupedDecisionPoint("joint", "CHECK", names(leaves))
  old <- lapply(names(leaves), function(id) {
    ActionEvent(paste0(id, "_old"), 6, decision_point_id = id)
  })
  names(old) <- names(leaves)
  plan <- DecisionPlan(list(
    null_member = NULL,
    keep_member = ActionEvent("keep_new", 7, decision_point_id = "keep_member"),
    replace_member = ActionEvent("replace_new", 7, decision_point_id = "replace_member"),
    warn_member = ActionEvent("warn_new", 7, decision_point_id = "warn_member")
  ))

  result <- fluxCore:::.preflight_group_pending_actions(plan, group, leaves, old)
  expect_identical(
    unname(result$outcomes),
    c("no_action", "keep", "replace", "warn")
  )
  expect_identical(result$warning_ids, "warn_member")
  expect_identical(result$candidate$null_member, old$null_member)
  expect_identical(result$candidate$keep_member, old$keep_member)
  expect_identical(result$candidate$replace_member$action_type, "replace_new")
  expect_identical(result$candidate$warn_member$action_type, "warn_new")
  expect_identical(old$replace_member$action_type, "replace_member_old")
})

test_that("an invalid later member emits no queued pending-replacement warning", {
  warnings <- character()
  leaves <- list(
    DecisionPoint(
      "a", NULL,
      allowed_actions = c("A_OLD", "A_NEW"),
      on_pending_action = "warn"
    ),
    DecisionPoint("b", NULL)
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  schema <- .s3b_schema(leaves, list(group))
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      if (entity$current$checks == 1L) {
        return(DecisionPlan(list(a = ActionEvent("A_OLD", 6), b = NULL)))
      }
      structure(
        list(
          selections = list(a = ActionEvent("A_NEW", 7), b = "invalid"),
          metadata = NULL
        ),
        class = "DecisionPlan"
      )
    }
  )
  engine <- load_model(
    schema,
    .s3b_bundle(
      n_checks = 2L,
      event_catalog = c("CHECK", "A_OLD", "A_NEW")
    ),
    policy = policy
  )

  err <- tryCatch(
    withCallingHandlers(
      engine$run(.s3b_entity(schema), max_events = 8),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "DecisionPoint\\('b'\\).*ActionEvent")
  expect_length(warnings, 0L)
})

test_that("a currently realized member action is retired before grouped preflight", {
  transition_calls <- 0L
  leaves <- list(
    DecisionPoint(
      "a", NULL,
      allowed_actions = "A",
      on_pending_action = "error"
    ),
    DecisionPoint("b", NULL)
  )
  group <- GroupedDecisionPoint("joint", c("CHECK", "A"), c("a", "b"))
  schema <- .s3b_schema(leaves, list(group))
  calls <- 0L
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      calls <<- calls + 1L
      selection <- switch(
        as.character(calls),
        `1` = ActionEvent("A", 2),
        `2` = ActionEvent("A", 3),
        NULL
      )
      DecisionPlan(list(a = selection, b = NULL))
    }
  )
  bundle <- .s3b_bundle(
    event_catalog = c("CHECK", "A"),
    transition_hook = function(entity, event) transition_calls <<- transition_calls + 1L
  )

  out <- load_model(schema, bundle, policy = policy)$run(
    .s3b_entity(schema), max_events = 8
  )
  expect_identical(calls, 3L)
  expect_identical(transition_calls, 3L)
  expect_identical(out$events$event_type, c("init", "CHECK", "A", "A"))
  expect_identical(out$events$time, c(0, 1, 2, 3))
})


# Adapter, mixed boundaries, and Q7 stochastic replay ----------------------

test_that("an explicit per-member adapter runs in declared order and stages a complete plan", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  adapter_calls <- character()
  select_one <- function(decision_point, entity) {
    adapter_calls <<- c(adapter_calls, decision_point$id)
    if (identical(decision_point$id, "a")) {
      ActionEvent("A", 2)
    } else {
      ActionEvent("B", 3)
    }
  }
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      selections <- lapply(
        eligible_decision_points,
        select_one,
        entity = entity
      )
      DecisionPlan(selections)
    }
  )

  out <- load_model(schema, .s3b_bundle(), policy = policy)$run(.s3b_entity(schema))
  expect_identical(adapter_calls, c("a", "b"))
  expect_identical(out$events$event_type, c("init", "CHECK", "A", "B"))
})

test_that("the explicit per-member adapter handles all-NULL and invalid member results", {
  declarations <- .s3b_basic_declarations()
  schema <- .s3b_schema(declarations$leaves, list(declarations$group))
  bundle <- .s3b_bundle()

  null_calls <- character()
  null_policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      selections <- lapply(eligible_decision_points, function(decision_point) {
        null_calls <<- c(null_calls, decision_point$id)
        NULL
      })
      DecisionPlan(selections)
    }
  )
  null_out <- load_model(schema, bundle, policy = null_policy)$run(
    .s3b_entity(schema)
  )
  expect_identical(null_calls, c("a", "b"))
  expect_identical(null_out$events$event_type, c("init", "CHECK"))

  invalid_calls <- character()
  invalid_policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      selections <- lapply(eligible_decision_points, function(decision_point) {
        invalid_calls <<- c(invalid_calls, decision_point$id)
        if (identical(decision_point$id, "a")) ActionEvent("A", 2) else "invalid"
      })
      DecisionPlan(selections)
    }
  )
  invalid_entity <- .s3b_entity(schema)
  expect_error(
    load_model(schema, bundle, policy = invalid_policy)$run(invalid_entity),
    "propose_plan.*DecisionPlan.*selection\\(s\\) \\{b\\}.*ActionEvent"
  )
  expect_identical(invalid_calls, c("a", "b"))
  expect_identical(invalid_entity$events$event_type, c("init", "CHECK"))
})

test_that("ordinary pending diagnostics occur before a later independent group failure", {
  warnings <- character()
  policy_order <- character()
  leaves <- list(
    DecisionPoint(
      "ordinary", "CHECK",
      allowed_actions = c("O_OLD", "O_NEW"),
      on_pending_action = "warn"
    ),
    DecisionPoint("a", NULL),
    DecisionPoint("b", NULL)
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  schema <- .s3b_schema(leaves, list(group))
  policy <- list(
    propose_action = function(decision_point, entity) {
      policy_order <<- c(policy_order, "ordinary")
      if (entity$current$checks == 1L) {
        ActionEvent("O_OLD", 6)
      } else {
        ActionEvent("O_NEW", 7)
      }
    },
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity) {
      policy_order <<- c(policy_order, "group")
      if (entity$current$checks == 1L) {
        .s3b_null_plan(eligible_decision_points)
      } else {
        NULL
      }
    }
  )
  bundle <- .s3b_bundle(
    n_checks = 2L,
    event_catalog = c("CHECK", "O_OLD", "O_NEW")
  )
  engine <- load_model(schema, bundle, policy = policy)

  err <- tryCatch(
    withCallingHandlers(
      engine$run(.s3b_entity(schema), max_events = 8),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "must be a DecisionPlan")
  expect_length(warnings, 1L)
  expect_match(warnings[[1]], "DecisionPoint\\('ordinary'\\) replaced")
  expect_identical(policy_order, c("ordinary", "group", "ordinary", "group"))
})

test_that("same cohort seed replays stochastic grouped plans without replicate collapse", {
  leaves <- list(
    DecisionPoint(
      "a", NULL,
      allowed_actions = c("A", "B")
    ),
    DecisionPoint("b", NULL, condition = function(entity) FALSE)
  )
  group <- GroupedDecisionPoint("joint", "CHECK", c("a", "b"))
  schema <- .s3b_schema(leaves, list(group))
  policy <- list(
    propose_plan = function(grouped_decision_point,
                            eligible_decision_points,
                            entity,
                            sim_ctx,
                            param_ctx) {
      action_type <- if (runif(1) < 0.5) "A" else "B"
      DecisionPlan(list(a = ActionEvent(action_type, 2)))
    }
  )
  engine <- load_model(
    schema,
    .s3b_bundle(event_catalog = c("CHECK", "A", "B")),
    policy = policy,
    runtime = RuntimeContext(seed = 111L)
  )
  run_batch <- function() {
    run_cohort(
      engine,
      entities = list(courier = .s3b_entity(schema, "courier")),
      n_sims = 10,
      seed = 90210L,
      backend = "none",
      max_events = 5
    )
  }
  first <- run_batch()
  second <- run_batch()
  selected <- function(batch) {
    vapply(
      batch$runs,
      function(run) tail(run$events$event_type, 1L),
      character(1)
    )
  }

  expect_identical(selected(first), selected(second))
  expect_gt(length(unique(selected(first))), 1L)
})

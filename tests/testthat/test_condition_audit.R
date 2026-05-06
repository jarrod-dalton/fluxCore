## Tests for DecisionPoint condition + audit parameters

# Reuse the stage 2B bundle/schema helpers defined in test_v2_stage2b.R.
# If those helpers are not visible (test isolation), replicate the minimal setup
# needed here.

.make_condition_bundle <- function() {
  propose_events <- function(entity, process_ids = NULL, current_proposals = NULL) {
    pid <- "default"
    if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
    t0 <- entity$last_time
    list(default = list(time_next = t0 + 1, event_type = "VISIT", process_id = pid))
  }
  transition <- function(entity, event) {
    if (identical(event$event_type, "ACT")) return(list(acted = TRUE))
    list()
  }
  stop_fn <- function(entity, event) isTRUE(entity$last_time >= 3)
  list(
    time_spec     = time_spec(unit = "years"),
    event_catalog = c("VISIT", "ACT"),
    propose_events = propose_events,
    transition     = transition,
    stop           = stop_fn,
    observe        = NULL,
    refresh_rules  = function(entity, last_event, changes) "ALL"
  )
}

.make_condition_entity <- function(flag = FALSE) {
  vars <- list(
    acted = list(type = "binary", levels = c("0", "1"), default = FALSE,
                 coerce = as.logical, validate = function(x) is.logical(x) && length(x) == 1L),
    flag  = list(type = "binary", levels = c("0", "1"), default = flag,
                 coerce = as.logical, validate = function(x) is.logical(x) && length(x) == 1L)
  )
  Entity$new(schema = vars, init = list(flag = flag))
}

# ---- condition = always FALSE -----------------------------------------------

test_that("condition=FALSE: policy never called", {
  dp <- DecisionPoint(
    id        = "cond_dp",
    trigger   = "VISIT",
    condition = function(entity) FALSE,
    allowed_actions = "ACT"
  )
  schema <- list(
    variables = list(acted = list(type="binary", levels=c("0","1"), default=FALSE,
                                  coerce=as.logical, validate=function(x) length(x)==1L && is.logical(x))),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
  calls <- 0L
  policy <- list(
    propose_action = function(decision_point, entity, ...) { calls <<- calls + 1L; NULL }
  )
  engine <- load_model(schema = schema, bundle = .make_condition_bundle(), policy = policy)
  entity <- Entity$new(schema = schema$variables)
  engine$run(entity = entity, max_events = 10)
  expect_equal(calls, 0L)
})

# ---- condition = always TRUE ------------------------------------------------

test_that("condition=TRUE: policy called same as no condition", {
  dp_cond <- DecisionPoint(
    id        = "cond_true",
    trigger   = "VISIT",
    condition = function(entity) TRUE,
    allowed_actions = "ACT"
  )
  dp_none <- DecisionPoint(
    id      = "cond_none",
    trigger = "VISIT",
    allowed_actions = "ACT"
  )
  make_schema <- function(dp) list(
    variables = list(acted = list(type="binary", levels=c("0","1"), default=FALSE,
                                  coerce=as.logical, validate=function(x) length(x)==1L && is.logical(x))),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
  calls_cond <- 0L
  calls_none <- 0L
  pol_cond <- list(propose_action = function(...) { calls_cond <<- calls_cond + 1L; NULL })
  pol_none <- list(propose_action = function(...) { calls_none <<- calls_none + 1L; NULL })

  bundle <- .make_condition_bundle()
  load_model(schema = make_schema(dp_cond), bundle = bundle, policy = pol_cond)$run(
    Entity$new(schema = make_schema(dp_cond)$variables), max_events = 5
  )
  load_model(schema = make_schema(dp_none), bundle = bundle, policy = pol_none)$run(
    Entity$new(schema = make_schema(dp_none)$variables), max_events = 5
  )
  expect_equal(calls_cond, calls_none)
})

# ---- audit = FALSE (default): no TrajectoryRecord when vetoed ---------------

test_that("audit=FALSE (default): no trajectory record emitted when condition is FALSE", {
  dp <- DecisionPoint(
    id        = "audit_false_dp",
    trigger   = "VISIT",
    condition = function(entity) FALSE,
    audit     = FALSE,
    allowed_actions = "ACT"
  )
  schema <- list(
    variables = list(acted = list(type="binary", levels=c("0","1"), default=FALSE,
                                  coerce=as.logical, validate=function(x) length(x)==1L && is.logical(x))),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
  policy <- list(propose_action = function(...) NULL)
  engine <- load_model(schema = schema, bundle = .make_condition_bundle(), policy = policy)
  entity <- Entity$new(schema = schema$variables)
  out <- engine$run(entity = entity, max_events = 5)
  expect_length(out$trajectory_records, 0L)
})

# ---- audit = TRUE: TrajectoryRecord emitted with condition_met = FALSE ------

test_that("audit=TRUE: trajectory record emitted with condition_met=FALSE when vetoed", {
  dp <- DecisionPoint(
    id        = "audit_true_dp",
    trigger   = "VISIT",
    condition = function(entity) FALSE,
    audit     = TRUE,
    allowed_actions = "ACT"
  )
  schema <- list(
    variables = list(acted = list(type="binary", levels=c("0","1"), default=FALSE,
                                  coerce=as.logical, validate=function(x) length(x)==1L && is.logical(x))),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
  policy <- list(propose_action = function(...) NULL)
  engine <- load_model(schema = schema, bundle = .make_condition_bundle(), policy = policy,
                       trajectory = list(detail = "summary"))
  entity <- Entity$new(schema = schema$variables)
  out <- engine$run(entity = entity, max_events = 3)
  expect_gt(length(out$trajectory_records), 0L)
  # All records should have condition_met = FALSE
  for (tr in out$trajectory_records) {
    expect_false(tr$condition_met)
    expect_null(tr$selected_action)
  }
})

# ---- audit=TRUE, active DP: condition_met = TRUE for normal fire ------------

test_that("audit=TRUE active DP: trajectory record has condition_met=TRUE", {
  dp <- DecisionPoint(
    id        = "audit_active_dp",
    trigger   = "VISIT",
    condition = function(entity) TRUE,
    audit     = TRUE,
    allowed_actions = "ACT"
  )
  schema <- list(
    variables = list(acted = list(type="binary", levels=c("0","1"), default=FALSE,
                                  coerce=as.logical, validate=function(x) length(x)==1L && is.logical(x))),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
  policy <- list(propose_action = function(...) NULL)
  engine <- load_model(schema = schema, bundle = .make_condition_bundle(), policy = policy,
                       trajectory = list(detail = "summary"))
  entity <- Entity$new(schema = schema$variables)
  out <- engine$run(entity = entity, max_events = 2)
  expect_gt(length(out$trajectory_records), 0L)
  for (tr in out$trajectory_records) {
    expect_true(tr$condition_met)
  }
})

# ---- no condition: condition_met field is NULL in TrajectoryRecord ----------

test_that("no condition: condition_met is NULL in trajectory record", {
  dp <- DecisionPoint(
    id      = "no_cond_dp",
    trigger = "VISIT",
    allowed_actions = "ACT"
  )
  schema <- list(
    variables = list(acted = list(type="binary", levels=c("0","1"), default=FALSE,
                                  coerce=as.logical, validate=function(x) length(x)==1L && is.logical(x))),
    time_spec = time_spec(unit = "years"),
    decision_points = list(dp)
  )
  policy <- list(propose_action = function(...) NULL)
  engine <- load_model(schema = schema, bundle = .make_condition_bundle(), policy = policy,
                       trajectory = list(detail = "summary"))
  entity <- Entity$new(schema = schema$variables)
  out <- engine$run(entity = entity, max_events = 2)
  expect_gt(length(out$trajectory_records), 0L)
  for (tr in out$trajectory_records) {
    expect_null(tr$condition_met)
  }
})

# ---- DecisionPoint() validation: bad condition/audit -----------------------

test_that("DecisionPoint errors on non-function condition", {
  expect_error(DecisionPoint("dp1", "ev", condition = "text"), "condition")
})

test_that("DecisionPoint errors on non-logical audit", {
  expect_error(DecisionPoint("dp1", "ev", audit = "yes"), "audit")
})

test_that("DecisionPoint errors on NA audit", {
  expect_error(DecisionPoint("dp1", "ev", audit = NA), "audit")
})

test_that("DecisionPoint constructs with condition and audit", {
  dp <- DecisionPoint(
    id        = "dp_cond",
    trigger   = "ev",
    condition = function(entity) entity$current$battery < 25,
    audit     = TRUE
  )
  expect_s3_class(dp, "DecisionPoint")
  expect_true(is.function(dp$condition))
  expect_true(dp$audit)
})

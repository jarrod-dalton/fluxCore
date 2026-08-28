## Action lifecycle tests (fluxCore #11)
##
## Covers the engine-owned action proposal store: realized actions are retired by
## the engine, pending actions survive model refresh, actions carry no process_id,
## and `last_event` reaches propose_events().
##
## Regression anchors:
##   D1 -- a realized action replayed forever under selective refresh.
##   D2 -- a pending action was destroyed by "ALL" refresh (the default).

# Helpers ------------------------------------------------------------------

.al_schema <- function(dps = NULL, event_catalog = c("tick", "act")) {
  sch <- list(
    variables = list(n = list(type = "count", default = 0L)),
    time_spec = time_spec(unit = "years"),
    event_catalog = event_catalog
  )
  if (!is.null(dps)) sch$decision_points <- dps
  sch
}

.al_bundle <- function(propose, refresh = function(entity, last_event, changes) "ALL",
                       event_catalog = c("tick", "act"),
                       stop_fn = function(entity, event) FALSE,
                       transition = function(entity, event) list()) {
  list(
    time_spec = time_spec(unit = "years"),
    event_catalog = event_catalog,
    propose_events = propose,
    transition = transition,
    stop = stop_fn,
    refresh_rules = refresh
  )
}

.al_entity <- function() Entity$new(schema = list(n = list(type = "count", default = 0L)))

.al_policy <- function(delay = 1, params = NULL, action = "act") {
  list(propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
    ActionEvent(action, time_next = entity$last_time + delay, params = params)
  })
}

# A model with a one-shot "trigger" plus a recurring "tick", used for the D1/D2
# regressions. The trigger fires once at t = 1 and then retreats out of range;
# `transition()` marks the entity so the trigger process retires itself.
.al_trigger_model <- function(refresh, action_delay, tick_every = 2) {
  sch <- .al_schema(
    dps = list(DecisionPoint(id = "dp", trigger = "trigger", allowed_actions = "act",
                             action_handlers = list(act = function(entity, event) list()))),
    event_catalog = c("trigger", "tick", "act")
  )
  bundle <- .al_bundle(
    propose = function(entity, process_ids = NULL, current_proposals = NULL) {
      all <- list(
        trigger_process = list(
          time_next = if (as.integer(entity$current$n) > 0L) 1e6 else 1,
          event_type = "trigger"
        ),
        tick_process = list(time_next = entity$last_time + tick_every, event_type = "tick")
      )
      if (is.null(process_ids)) return(all)
      all[intersect(names(all), process_ids)]
    },
    refresh = refresh,
    event_catalog = c("trigger", "tick", "act"),
    transition = function(entity, event) {
      if (identical(event$event_type, "trigger")) return(list(n = 1L))
      list()
    }
  )
  engine <- load_model(schema = sch, bundle = bundle, policy = .al_policy(delay = action_delay))
  engine$run(entity = .al_entity(), max_events = 8)
}


# D1 -- realized action must not replay ------------------------------------

test_that("a realized action is retired by the engine under selective refresh", {
  out <- .al_trigger_model(
    refresh = function(entity, last_event, changes) {
      if (identical(last_event$event_type, "act")) "tick_process" else "ALL"
    },
    action_delay = 0.1
  )
  expect_equal(sum(out$events$event_type == "act"), 1L)
})

test_that("a realized action is retired under full refresh as well", {
  out <- .al_trigger_model(
    refresh = function(entity, last_event, changes) "ALL",
    action_delay = 0.1
  )
  expect_equal(sum(out$events$event_type == "act"), 1L)
})


# D2 -- pending action must survive refresh --------------------------------

test_that("a pending action survives default 'ALL' refresh and still fires", {
  # Action is scheduled for t = 6; ticks at t = 3, 5 intervene before it.
  out <- .al_trigger_model(
    refresh = function(entity, last_event, changes) "ALL",
    action_delay = 5
  )
  expect_equal(sum(out$events$event_type == "act"), 1L)
  expect_equal(out$events$time[out$events$event_type == "act"], 6)
})

test_that("a pending action survives selective refresh and still fires", {
  out <- .al_trigger_model(
    refresh = function(entity, last_event, changes) c("trigger_process", "tick_process"),
    action_delay = 5
  )
  expect_equal(sum(out$events$event_type == "act"), 1L)
  expect_equal(out$events$time[out$events$event_type == "act"], 6)
})

test_that("action lifetime does not depend on the refresh strategy", {
  all_refresh <- .al_trigger_model(function(entity, last_event, changes) "ALL", 5)
  selective <- .al_trigger_model(
    function(entity, last_event, changes) c("trigger_process", "tick_process"), 5
  )
  expect_equal(all_refresh$events$event_type, selective$events$event_type)
  expect_equal(all_refresh$events$time, selective$events$time)
})


# Multiple and chained decision points -------------------------------------

test_that("two decision points firing on one event both realize, in time order", {
  sch <- .al_schema(
    dps = list(
      DecisionPoint(id = "early", trigger = "tick", allowed_actions = "act",
                    action_handlers = list(act = function(entity, event) list())),
      DecisionPoint(id = "late", trigger = "tick", allowed_actions = "act",
                    action_handlers = list(act = function(entity, event) list()))
    )
  )
  # Distinct offsets so the ordering is unambiguous.
  policy <- list(propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
    off <- if (identical(decision_point$id, "early")) 0.2 else 0.6
    ActionEvent("act", time_next = entity$last_time + off)
  })
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    list(tk = list(time_next = entity$last_time + 10, event_type = "tick"))
  })
  out <- load_model(schema = sch, bundle = bundle, policy = policy)$run(.al_entity(), max_events = 3)
  acts <- out$events[out$events$event_type == "act", ]
  expect_equal(nrow(acts), 2L)
  expect_equal(acts$time, c(10.2, 10.6))
})

test_that("a decision point can be triggered by another decision point's action", {
  sch <- .al_schema(
    dps = list(
      DecisionPoint(id = "first", trigger = "tick", allowed_actions = "step1",
                    action_handlers = list(step1 = function(entity, event) list())),
      DecisionPoint(id = "second", trigger = "step1", allowed_actions = "step2",
                    action_handlers = list(step2 = function(entity, event) list()))
    ),
    event_catalog = c("tick", "step1", "step2")
  )
  policy <- list(propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
    if (identical(decision_point$id, "first")) {
      ActionEvent("step1", time_next = entity$last_time + 1)
    } else {
      ActionEvent("step2", time_next = entity$last_time + 1)
    }
  })
  bundle <- .al_bundle(
    function(entity, process_ids = NULL) list(tk = list(time_next = entity$last_time + 50, event_type = "tick")),
    event_catalog = c("tick", "step1", "step2")
  )
  out <- load_model(schema = sch, bundle = bundle, policy = policy)$run(.al_entity(), max_events = 4)
  expect_true("step1" %in% out$events$event_type)
  expect_true("step2" %in% out$events$event_type)
  # The chain advances time: tick -> step1 -> step2.
  expect_equal(
    out$events$event_type[out$events$j %in% 1:3],
    c("tick", "step1", "step2")
  )
})

test_that("a chained action type missing from event_catalog errors clearly", {
  # "step1" has a NULL handler, so it is not auto-registered into the catalog.
  sch <- .al_schema(
    dps = list(DecisionPoint(id = "first", trigger = "tick", allowed_actions = "step1")),
    event_catalog = c("tick")
  )
  bundle <- .al_bundle(
    function(entity, process_ids = NULL) list(tk = list(time_next = entity$last_time + 1, event_type = "tick")),
    event_catalog = c("tick")
  )
  policy <- .al_policy(delay = 0.5, action = "step1")
  expect_error(
    load_model(schema = sch, bundle = bundle, policy = policy)$run(.al_entity(), max_events = 3),
    "not declared in bundle\\$event_catalog"
  )
})

test_that("proposal validation identifies actions by decision point, not process_id", {
  sch <- .al_schema(
    dps = list(DecisionPoint(id = "mydp", trigger = "tick", allowed_actions = "step1")),
    event_catalog = c("tick")
  )
  bundle <- .al_bundle(
    function(entity, process_ids = NULL) list(tk = list(time_next = entity$last_time + 1, event_type = "tick")),
    event_catalog = c("tick")
  )
  expect_error(
    load_model(schema = sch, bundle = bundle, policy = .al_policy(0.5, action = "step1"))$run(
      .al_entity(), max_events = 3
    ),
    "DecisionPoint\\('mydp'\\)"
  )
})


# Action identity ----------------------------------------------------------

test_that("a realized action carries no process_id and is identified by decision point", {
  seen <- list()
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "dp", trigger = "tick", allowed_actions = "act",
                  action_handlers = list(act = function(entity, event) list()))
  ))
  bundle <- .al_bundle(
    propose = function(entity, process_ids = NULL) {
      list(tk = list(time_next = entity$last_time + 10, event_type = "tick"))
    },
    refresh = function(entity, last_event, changes) {
      seen[[length(seen) + 1L]] <<- list(
        type = last_event$event_type,
        pid = last_event$process_id,
        dp = last_event$decision_point_id
      )
      "ALL"
    }
  )
  load_model(schema = sch, bundle = bundle, policy = .al_policy(1))$run(.al_entity(), max_events = 2)

  tick <- Filter(function(x) identical(x$type, "tick"), seen)[[1]]
  act <- Filter(function(x) identical(x$type, "act"), seen)[[1]]

  expect_equal(tick$pid, "tk")
  expect_null(tick$dp)
  expect_null(act$pid)
  expect_equal(act$dp, "dp")
})

test_that("policy action provenance is filled or accepted when it matches ownership", {
  run_case <- function(decision_point_id) {
    seen_id <- NULL
    dp <- DecisionPoint(
      id = "owner",
      trigger = "tick",
      allowed_actions = "act",
      action_handlers = list(act = function(entity, event) {
        seen_id <<- event$decision_point_id
        list(n = as.integer(entity$current$n) + 1L)
      })
    )
    policy <- list(propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
      ActionEvent(
        "act",
        time_next = entity$last_time + 0.5,
        decision_point_id = decision_point_id
      )
    })
    bundle <- .al_bundle(function(entity, process_ids = NULL) {
      list(tk = list(time_next = entity$last_time + 10, event_type = "tick"))
    })
    out <- load_model(schema = .al_schema(dps = list(dp)), bundle = bundle, policy = policy)$run(
      .al_entity(), max_events = 2
    )
    list(out = out, seen_id = seen_id)
  }

  filled <- run_case(NULL)
  matched <- run_case("owner")

  expect_identical(filled$out$entity$current$n, 1L)
  expect_identical(matched$out$entity$current$n, 1L)
  expect_identical(filled$seen_id, "owner")
  expect_identical(matched$seen_id, "owner")
})

test_that("policy action provenance cannot contradict DecisionPoint ownership", {
  dp <- DecisionPoint(
    id = "owner",
    trigger = "tick",
    allowed_actions = "act",
    action_handlers = list(act = function(entity, event) list(n = 99L))
  )
  policy <- list(propose_action = function(decision_point, entity, sim_ctx, param_ctx) {
    ActionEvent(
      "act",
      time_next = entity$last_time + 0.5,
      decision_point_id = "other"
    )
  })
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    list(tk = list(time_next = entity$last_time + 10, event_type = "tick"))
  })
  entity <- .al_entity()

  expect_error(
    load_model(schema = .al_schema(dps = list(dp)), bundle = bundle, policy = policy)$run(
      entity, max_events = 2
    ),
    "decision_point_id 'other'.*DecisionPoint id is 'owner'"
  )
  expect_identical(entity$current$n, 0L)
  expect_equal(entity$events$event_type, c("init", "tick"))
})

test_that("a model process may share a name with a decision point without colliding", {
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "dp", trigger = "tick", allowed_actions = "act",
                  action_handlers = list(act = function(entity, event) list()))
  ))
  # The model process is *also* called "dp".
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    list(dp = list(time_next = entity$last_time + 2, event_type = "tick"))
  })
  out <- load_model(schema = sch, bundle = bundle, policy = .al_policy(1))$run(.al_entity(), max_events = 6)
  # Ticks and actions strictly alternate; neither store corrupts the other.
  expect_equal(out$events$event_type[-1], rep(c("tick", "act"), 3))
})


# Reserved namespace -------------------------------------------------------

test_that("propose_events rejects reserved dot-prefixed process ids", {
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    stats::setNames(list(list(time_next = 1, event_type = "tick")), ".action.dp")
  })
  expect_error(
    Engine$new(bundle = bundle)$run(.al_entity(), max_events = 2),
    "reserved process_id"
  )
})

test_that("refresh_rules rejects reserved dot-prefixed process ids", {
  bundle <- .al_bundle(
    propose = function(entity, process_ids = NULL) {
      list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
    },
    refresh = function(entity, last_event, changes) ".action.dp"
  )
  expect_error(
    Engine$new(bundle = bundle)$run(.al_entity(), max_events = 2),
    "reserved process_id"
  )
})


# on_pending_action --------------------------------------------------------

.al_pending_model <- function(mode = NULL) {
  dp_args <- list(id = "dp", trigger = "tick", allowed_actions = "act",
                  action_handlers = list(act = function(entity, event) list()))
  if (!is.null(mode)) dp_args$on_pending_action <- mode
  sch <- .al_schema(dps = list(do.call(DecisionPoint, dp_args)))
  # Exactly two triggers compete: the t=1 decision proposes t=6 and the t=2
  # decision proposes t=7. No model process remains after the second trigger,
  # so whichever action is retained must be the next realized event.
  bundle <- .al_bundle(
    propose = function(entity, process_ids = NULL) {
      n <- as.integer(entity$current$n)
      if (n >= 2L) return(list())
      list(tk = list(time_next = n + 1, event_type = "tick"))
    },
    stop_fn = function(entity, event) identical(event$event_type, "act"),
    transition = function(entity, event) {
      if (identical(event$event_type, "tick")) {
        return(list(n = as.integer(entity$current$n) + 1L))
      }
      list()
    }
  )
  load_model(schema = sch, bundle = bundle, policy = .al_policy(5))$run(
    .al_entity(), max_events = 4
  )
}

test_that("default pending mode warns once and realizes the newer action", {
  warnings <- character()
  out <- withCallingHandlers(
    .al_pending_model(NULL),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, "replaced a still-pending action")
  expect_equal(sum(out$events$event_type == "act"), 1L)
  expect_equal(out$events$time[out$events$event_type == "act"], 7)
})

test_that("on_pending_action = 'replace' silently realizes the newer action", {
  expect_no_warning(out <- .al_pending_model("replace"))
  expect_equal(sum(out$events$event_type == "act"), 1L)
  expect_equal(out$events$time[out$events$event_type == "act"], 7)
})

test_that("on_pending_action = 'keep' silently realizes the earlier action", {
  expect_no_warning(out <- .al_pending_model("keep"))
  expect_equal(sum(out$events$event_type == "act"), 1L)
  expect_equal(out$events$time[out$events$event_type == "act"], 6)
})

test_that("on_pending_action = 'error' aborts the second proposal", {
  expect_error(.al_pending_model("error"), "still pending")
})

test_that("re-proposing after one's own action fired is not a conflict", {
  # Action resolves at +0.2, well before the next tick at +1, so nothing is ever
  # pending when the decision point proposes again.
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "dp", trigger = "tick", allowed_actions = "act",
                  action_handlers = list(act = function(entity, event) list()))
  ))
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
  })
  expect_silent(
    load_model(schema = sch, bundle = bundle, policy = .al_policy(0.2))$run(.al_entity(), max_events = 6)
  )
})

test_that("DecisionPoint rejects an unknown on_pending_action", {
  expect_error(
    DecisionPoint(id = "dp", trigger = "tick", on_pending_action = "nonsense"),
    "should be one of"
  )
})


# Duplicate decision point ids ---------------------------------------------

test_that("load_model rejects duplicated DecisionPoint ids", {
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "same", trigger = "tick", allowed_actions = "act"),
    DecisionPoint(id = "same", trigger = "tick", allowed_actions = "act")
  ))
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    list(tk = list(time_next = 1, event_type = "tick"))
  })
  expect_error(load_model(schema = sch, bundle = bundle), "duplicated DecisionPoint id")
})

test_that("load_model accepts distinct DecisionPoint ids", {
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "a", trigger = "tick", allowed_actions = "act"),
    DecisionPoint(id = "b", trigger = "tick", allowed_actions = "act")
  ))
  bundle <- .al_bundle(function(entity, process_ids = NULL) {
    list(tk = list(time_next = 1, event_type = "tick"))
  })
  expect_s3_class(load_model(schema = sch, bundle = bundle), "Engine")
})


# last_event ---------------------------------------------------------------

test_that("propose_events receives NULL last_event on initial generation", {
  seen <- list()
  bundle <- .al_bundle(function(entity, process_ids = NULL, last_event = NULL) {
    # Wrap in a list: assigning NULL into a list element would delete it, not append.
    seen[[length(seen) + 1L]] <<- list(ev = last_event)
    list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
  })
  Engine$new(bundle = bundle)$run(.al_entity(), max_events = 3)
  expect_null(seen[[1]]$ev)
  expect_false(is.null(seen[[2]]$ev))
  expect_equal(seen[[2]]$ev$event_type, "tick")
})

test_that("propose_events receives the full realized action, including params", {
  seen <- list()
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "dp", trigger = "tick", allowed_actions = "act",
                  action_handlers = list(act = function(entity, event) list()))
  ))
  bundle <- .al_bundle(function(entity, process_ids = NULL, last_event = NULL) {
    if (identical(last_event$event_type, "act")) seen[[length(seen) + 1L]] <<- last_event
    list(tk = list(time_next = entity$last_time + 10, event_type = "tick"))
  })
  policy <- .al_policy(1, params = list(delay_days = 30))
  load_model(schema = sch, bundle = bundle, policy = policy)$run(.al_entity(), max_events = 3)

  expect_gt(length(seen), 0L)
  expect_equal(seen[[1]]$params$delay_days, 30)
  expect_equal(seen[[1]]$decision_point_id, "dp")
  expect_s3_class(seen[[1]], "ActionEvent")
})

test_that("last_event reaches propose_events under selective refresh too", {
  seen <- list()
  bundle <- .al_bundle(
    propose = function(entity, process_ids = NULL, current_proposals = NULL, last_event = NULL) {
      if (!is.null(last_event)) seen[[length(seen) + 1L]] <<- last_event$event_type
      list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
    },
    refresh = function(entity, last_event, changes) "tk"
  )
  Engine$new(bundle = bundle)$run(.al_entity(), max_events = 3)
  expect_equal(unique(unlist(seen)), "tick")
})

test_that("an action's params can reschedule a process with no transport state variable", {
  sch <- .al_schema(dps = list(
    DecisionPoint(id = "dp", trigger = "tick", allowed_actions = "act")
  ))
  bundle <- .al_bundle(function(entity, process_ids = NULL, last_event = NULL) {
    if (identical(last_event$event_type, "act")) {
      return(list(tk = list(time_next = entity$last_time + last_event$params$delay_days,
                            event_type = "tick")))
    }
    list(tk = list(time_next = entity$last_time + 3, event_type = "tick"))
  })
  out <- load_model(
    schema = sch, bundle = bundle,
    policy = .al_policy(1, params = list(delay_days = 30))
  )$run(.al_entity(), max_events = 3)

  # tick at 3, act at 4, then the next tick is pushed to 4 + 30 = 34.
  expect_equal(out$events$time, c(0, 3, 4, 34))
  # The schema never carried a variable used purely to ferry delay_days.
  expect_equal(names(out$entity$current), "n")
})

test_that("a propose_events not declaring last_event is unaffected", {
  bundle_without <- .al_bundle(function(entity, process_ids = NULL) {
    list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
  })
  bundle_with <- .al_bundle(function(entity, process_ids = NULL, last_event = NULL) {
    list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
  })
  a <- Engine$new(bundle = bundle_without)$run(.al_entity(), max_events = 4)
  b <- Engine$new(bundle = bundle_with)$run(.al_entity(), max_events = 4)
  expect_equal(a$events, b$events)
})


# stopped_by ---------------------------------------------------------------

test_that("stopped_by reports why the run ended", {
  ticker <- function(entity, process_ids = NULL) {
    list(tk = list(time_next = entity$last_time + 1, event_type = "tick"))
  }

  by_stop <- Engine$new(bundle = .al_bundle(
    ticker, stop_fn = function(entity, event) entity$last_time >= 3
  ))$run(.al_entity(), max_events = 50)
  expect_equal(by_stop$stopped_by, "stop")

  by_events <- Engine$new(bundle = .al_bundle(ticker))$run(.al_entity(), max_events = 3)
  expect_equal(by_events$stopped_by, "max_events")

  by_time <- Engine$new(bundle = .al_bundle(ticker))$run(.al_entity(), max_events = 50, max_time = 4)
  expect_equal(by_time$stopped_by, "max_time")

  # A model whose only process withdraws its proposal leaves nothing to schedule.
  exhausting <- .al_bundle(
    propose = function(entity, process_ids = NULL) {
      if (entity$last_j > 0L) return(list())
      list(tk = list(time_next = 1, event_type = "tick"))
    }
  )
  by_empty <- Engine$new(bundle = exhausting)$run(.al_entity(), max_events = 50)
  expect_equal(by_empty$stopped_by, "no_proposals")

  # The same reason applies when initial proposal generation is already empty.
  initially_empty <- .al_bundle(function(entity, process_ids = NULL) list())
  by_initial_empty <- Engine$new(bundle = initially_empty)$run(.al_entity(), max_events = 50)
  expect_equal(by_initial_empty$stopped_by, "no_proposals")
  expect_equal(nrow(by_initial_empty$events), 1L)
})

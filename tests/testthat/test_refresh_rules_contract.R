make_refresh_bundle <- function(refresh_rules = NULL) {
  calls <- list()

  bundle <- list(
    time_spec = time_spec(unit = "days"),
    propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      calls <<- append(calls, list(process_ids))

      pids <- if (is.null(process_ids)) c("p1", "p2") else process_ids
      out <- vector("list", length(pids))
      names(out) <- pids
      for (k in seq_along(pids)) {
        pid <- pids[[k]]
        out[[k]] <- list(
          time_next = entity$last_time + if (identical(pid, "p1")) 1 else 2,
          event_type = "tick"
        )
      }
      out
    },
    transition = function(entity, event, ctx = NULL) NULL,
    stop = function(entity, event, ctx = NULL) entity$j >= 3L
  )

  if (!is.null(refresh_rules)) bundle$refresh_rules <- refresh_rules

  list(
    bundle = bundle,
    get_calls = function() calls
  )
}

test_that("missing refresh_rules defaults to ALL refresh behavior", {
  x <- make_refresh_bundle(refresh_rules = NULL)
  prov <- PackageProvider$new(registry = list(x = function() x$bundle))
  eng <- Engine$new(provider = prov, model_spec = list(name = "x"))
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_no_error(eng$run(entity = p, max_events = 3, return_observations = FALSE))

  calls <- x$get_calls()
  expect_true(length(calls) >= 2L)
  expect_null(calls[[1L]])
  expect_null(calls[[2L]])
})

test_that("refresh_rules rejects non-character return types", {
  x <- make_refresh_bundle(
    refresh_rules = function(entity, last_event, changes, ctx = NULL) TRUE
  )
  prov <- PackageProvider$new(registry = list(x = function() x$bundle))
  eng <- Engine$new(provider = prov, model_spec = list(name = "x"))
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng$run(entity = p, max_events = 3, return_observations = FALSE),
    "must return exactly \"ALL\" or a character vector"
  )
})

test_that("refresh_rules rejects ALL mixed with other process ids", {
  x <- make_refresh_bundle(
    refresh_rules = function(entity, last_event, changes, ctx = NULL) c("ALL", "p1")
  )
  prov <- PackageProvider$new(registry = list(x = function() x$bundle))
  eng <- Engine$new(provider = prov, model_spec = list(name = "x"))
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng$run(entity = p, max_events = 3, return_observations = FALSE),
    "may return \"ALL\" only as a single scalar value"
  )
})

test_that("refresh_rules rejects empty and duplicate process ids", {
  x1 <- make_refresh_bundle(
    refresh_rules = function(entity, last_event, changes, ctx = NULL) c("p1", "")
  )
  prov1 <- PackageProvider$new(registry = list(x = function() x1$bundle))
  eng1 <- Engine$new(provider = prov1, model_spec = list(name = "x"))
  p1 <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng1$run(entity = p1, max_events = 3, return_observations = FALSE),
    "empty or missing process_id values"
  )

  x2 <- make_refresh_bundle(
    refresh_rules = function(entity, last_event, changes, ctx = NULL) c("p1", "p1")
  )
  prov2 <- PackageProvider$new(registry = list(x = function() x2$bundle))
  eng2 <- Engine$new(provider = prov2, model_spec = list(name = "x"))
  p2 <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_error(
    eng2$run(entity = p2, max_events = 3, return_observations = FALSE),
    "duplicated process_id values"
  )
})

test_that("refresh_rules accepts character vector of process ids", {
  x <- make_refresh_bundle(
    refresh_rules = function(entity, last_event, changes, ctx = NULL) "p1"
  )
  prov <- PackageProvider$new(registry = list(x = function() x$bundle))
  eng <- Engine$new(provider = prov, model_spec = list(name = "x"))
  p <- Entity$new(init = list(alive = TRUE), schema = default_entity_schema(), time0 = 0)

  expect_no_error(eng$run(entity = p, max_events = 3, return_observations = FALSE))
})

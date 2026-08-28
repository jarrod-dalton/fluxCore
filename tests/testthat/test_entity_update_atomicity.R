.q3_atomic_schema <- function() {
  list(
    x = list(type = "nonnegative_integer", default = 1L),
    y = list(type = "nonnegative_integer", default = 2L),
    coercion_probe = list(
      type = "nonnegative_integer",
      default = 3L,
      coerce = function(value) {
        if (identical(value, "explode")) stop("coercion exploded", call. = FALSE)
        as.integer(value)
      }
    )
  )
}

.q3_coupled_state <- function(entity) {
  list(
    events = entity$events,
    last_j = entity$last_j,
    last_time = entity$last_time,
    current = entity$current,
    hist = entity$hist
  )
}

.q3_expect_coupled_state <- function(entity, expected) {
  expect_identical(entity$events, expected$events)
  expect_identical(entity$last_j, expected$last_j)
  expect_identical(entity$last_time, expected$last_time)
  expect_identical(entity$current, expected$current)
  expect_identical(entity$hist, expected$hist)
}

test_that("Entity$update is atomic when state validation rejects a patch", {
  entity <- Entity$new(schema = .q3_atomic_schema())
  before <- .q3_coupled_state(entity)

  expect_error(
    entity$update(time = 1, event_type = "bad_single", changes = list(x = -1L)),
    "Value for 'x' must be a non-negative integer.",
    fixed = TRUE
  )
  .q3_expect_coupled_state(entity, before)

  expect_error(
    entity$update(
      time = 1,
      event_type = "bad_late_field",
      changes = list(x = 9L, y = -1L)
    ),
    "Value for 'y' must be a non-negative integer.",
    fixed = TRUE
  )
  .q3_expect_coupled_state(entity, before)
})

test_that("Entity$update is atomic for malformed and unknown patches", {
  cases <- list(
    list(
      changes = 2L,
      message = "changes must be a named list or NULL."
    ),
    list(
      changes = list(2L),
      message = "changes must be a *named* list or NULL."
    ),
    list(
      changes = structure(list(2L, 3L), names = c("x", "")),
      message = "changes must be a *named* list or NULL."
    ),
    list(
      changes = structure(list(2L, 3L), names = c("x", "x")),
      message = "changes must not contain duplicate names."
    ),
    list(
      changes = list(unknown = 2L),
      message = "changes contained unknown state vars: unknown"
    )
  )

  for (case in cases) {
    entity <- Entity$new(schema = .q3_atomic_schema())
    before <- .q3_coupled_state(entity)

    expect_error(
      entity$update(time = 1, event_type = "bad_shape", changes = case$changes),
      case$message,
      fixed = TRUE
    )
    .q3_expect_coupled_state(entity, before)
  }
})

test_that("Entity$update is atomic when coercion errors", {
  entity <- Entity$new(schema = .q3_atomic_schema())
  before <- .q3_coupled_state(entity)

  expect_error(
    entity$update(
      time = 1,
      event_type = "bad_coercion",
      changes = list(x = 8L, coercion_probe = "explode")
    ),
    "coercion exploded",
    fixed = TRUE
  )
  .q3_expect_coupled_state(entity, before)
})

test_that("Entity$update preserves successful multi-field and NULL behavior", {
  entity <- Entity$new(schema = .q3_atomic_schema(), time0 = 0)

  expect_invisible(
    entity$update(
      time = 2.5,
      event_type = "multi",
      changes = list(x = 5L, y = 7L)
    )
  )
  expect_identical(entity$last_j, 1L)
  expect_identical(entity$last_time, 2.5)
  expect_identical(entity$current$x, 5L)
  expect_identical(entity$current$y, 7L)
  expect_identical(entity$hist$x, list(j = c(0L, 1L), v = c(1L, 5L)))
  expect_identical(entity$hist$y, list(j = c(0L, 1L), v = c(2L, 7L)))
  expect_identical(entity$hist$coercion_probe, list(j = 0L, v = 3L))
  expect_identical(
    entity$events,
    data.frame(
      j = c(0L, 1L),
      time = c(0, 2.5),
      event_type = c("init", "multi"),
      stringsAsFactors = FALSE
    )
  )

  state_after_multi <- entity$current
  hist_after_multi <- entity$hist
  expect_invisible(entity$update(time = 3, event_type = "no_effect", changes = NULL))

  expect_identical(entity$last_j, 2L)
  expect_identical(entity$last_time, 3)
  expect_identical(entity$current, state_after_multi)
  expect_identical(entity$hist, hist_after_multi)
  expect_identical(
    entity$events,
    data.frame(
      j = c(0L, 1L, 2L),
      time = c(0, 2.5, 3),
      event_type = c("init", "multi", "no_effect"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("Engine transition rejection leaves no phantom entity event", {
  entity <- Entity$new(
    schema = list(x = list(type = "nonnegative_integer", default = 1L))
  )
  before <- .q3_coupled_state(entity)
  bundle <- list(
    time_spec = time_spec(unit = "hours"),
    event_catalog = "invalid_transition",
    propose_events = function(entity) {
      list(
        dispatch = list(
          time_next = entity$last_time + 1,
          event_type = "invalid_transition"
        )
      )
    },
    transition = function(entity, event) list(x = -1L),
    stop = function(entity, event) FALSE
  )
  engine <- Engine$new(bundle = bundle)

  expect_error(
    engine$run(entity, max_events = 1, return_observations = FALSE),
    "Value for 'x' must be a non-negative integer.",
    fixed = TRUE
  )
  .q3_expect_coupled_state(entity, before)
})

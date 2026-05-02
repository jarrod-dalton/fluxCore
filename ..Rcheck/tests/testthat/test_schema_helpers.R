test_that("Schema helpers validate and lookup metadata", {
  schema <- list(
    x = list(type = "numeric", default = 0, coerce = as.numeric),
    y = list(type = "categorical", levels = c("a", "b"), default = "a", coerce = as.character),
    z = list(type = "binary", levels = c("0", "1"), default = TRUE, coerce = as.logical, blocks = c("vitals"))
  )

  expect_silent(schema_validate(schema))
  expect_silent(schema_assert_vars(schema, c("x", "y")))
  expect_error(schema_assert_vars(schema, c("nope")), "Unknown schema variable", fixed = TRUE)

  info <- schema_var_info(schema, c("x", "y", "z"))
  expect_equal(info$type, c("numeric", "categorical", "binary"))
  expect_true(is.list(info$levels))
  expect_true(is.list(info$blocks))

  expect_silent(schema_assert_types(schema, c("x"), allowed_types = c("numeric")))
  expect_error(schema_assert_types(schema, c("x"), allowed_types = c("categorical")), "incompatible type", fixed = TRUE)

  expect_silent(schema_assert_levels(schema, "y", c("a")))
  expect_error(schema_assert_levels(schema, "y", c("c")), "not declared", fixed = TRUE)
  expect_error(schema_assert_levels(schema, "x", c("a")), "does not declare", fixed = TRUE)
})

test_that("schema validator presets support numeric, integer, and level checks", {
  numeric_check <- schema_validator_numeric(min = 0, max = 5, allow_na = TRUE)
  expect_true(numeric_check(0))
  expect_true(numeric_check(5))
  expect_true(numeric_check(NA_real_))
  expect_false(numeric_check(-1))
  expect_false(numeric_check(6))
  expect_false(numeric_check(c(1, 2)))

  integer_check <- schema_validator_integer(min = 0, max = 2, allow_na = FALSE)
  expect_true(integer_check(2))
  expect_false(integer_check(1.5))
  expect_false(integer_check(NA_real_))

  level_check <- schema_validator_levels(c("a", "b"), allow_na = TRUE)
  expect_true(level_check("a"))
  expect_true(level_check(NA_character_))
  expect_false(level_check("c"))
})

test_that("schema validation metadata is enforced during Entity initialization and updates", {
  schema <- list(
    x = list(type = "numeric", default = 1, coerce = as.numeric, min = 0, max = 10),
    y = list(type = "categorical", levels = c("a", "b"), default = "a", coerce = as.character, required = TRUE, allow_na = TRUE)
  )

  expect_silent(schema_validate(schema))
  expect_error(Entity$new(init = list(), schema = schema, time0 = 0), "Missing required init state var 'y'", fixed = TRUE)
  expect_error(Entity$new(init = list(x = -1, y = "a"), schema = schema, time0 = 0), "Value for 'x' must be >= 0", fixed = TRUE)
  expect_error(Entity$new(init = list(x = 1, y = "c"), schema = schema, time0 = 0), "Value for 'y' must be one of:", fixed = TRUE)

  p <- Entity$new(init = list(x = 0, y = "a"), schema = schema, time0 = 0)
  expect_error(p$update(time = 1, event_type = "visit", changes = list(x = 11)), "Value for 'x' must be <= 10", fixed = TRUE)
})

test_that("new type presets enforce correct validation", {
  schema <- list(
    logical_var = list(type = "logical", default = TRUE, coerce = as.logical, allow_na = TRUE),
    binary_var = list(type = "binary", levels = c("0", "1"), default = TRUE, coerce = as.logical, allow_na = TRUE),
    integer_var = list(type = "integer", default = 1L, coerce = as.integer, allow_na = TRUE),
    count_var = list(type = "count", default = 0L, coerce = as.integer, allow_na = TRUE),
    nonnegative_int = list(type = "nonnegative_integer", default = 0L, coerce = as.integer, allow_na = TRUE),
    positive_int = list(type = "positive_integer", default = 1L, coerce = as.integer, allow_na = TRUE),
    numeric_var = list(type = "numeric", default = 1.0, coerce = as.numeric, allow_na = TRUE),
    nonnegative_num = list(type = "nonnegative_numeric", default = 0.0, coerce = as.numeric, allow_na = TRUE),
    positive_num = list(type = "positive_numeric", default = 1.0, coerce = as.numeric, allow_na = TRUE),
    prob_var = list(type = "probability", default = 0.5, coerce = as.numeric, allow_na = TRUE),
    string_var = list(type = "string", default = "test", coerce = as.character, allow_na = TRUE),
    nonempty_str = list(type = "nonempty_string", default = "test", coerce = as.character, allow_na = TRUE)
  )

  expect_silent(schema_validate(schema))

  # Test valid values
  p <- Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.1,
    prob_var = 0.7,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0)

  # Test invalid values
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = -1L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.1,
    prob_var = 0.7,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0), "must be a non-negative integer")
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 0L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.1,
    prob_var = 0.7,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0), "must be a positive integer")
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = -0.1,
    positive_num = 0.1,
    prob_var = 0.7,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0), "must be a non-negative numeric")
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.0,
    prob_var = 0.7,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0), "must be a positive numeric")
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.1,
    prob_var = 1.5,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0), "must be a probability")
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.1,
    prob_var = -0.1,
    string_var = "hello",
    nonempty_str = "world"
  ), schema = schema, time0 = 0), "must be a probability")
  expect_error(Entity$new(init = list(
    logical_var = FALSE,
    binary_var = TRUE,
    integer_var = 2L,
    count_var = 5L,
    nonnegative_int = 0L,
    positive_int = 2L,
    numeric_var = 3.14,
    nonnegative_num = 0.0,
    positive_num = 0.1,
    prob_var = 0.7,
    string_var = "hello",
    nonempty_str = ""
  ), schema = schema, time0 = 0), "must be a non-empty character string")
})

test_that("percent type accepts [0,100] and rejects out-of-range / non-numeric", {
  schema <- list(
    pct = list(type = "percent", default = 50, coerce = as.numeric)
  )
  expect_silent(schema_validate(schema))
  expect_silent(Entity$new(init = list(pct = 0),   schema = schema, time0 = 0))
  expect_silent(Entity$new(init = list(pct = 50),  schema = schema, time0 = 0))
  expect_silent(Entity$new(init = list(pct = 100), schema = schema, time0 = 0))
  expect_error(Entity$new(init = list(pct = -1),  schema = schema, time0 = 0), "must be a percent")
  expect_error(Entity$new(init = list(pct = 101), schema = schema, time0 = 0), "must be a percent")
})

test_that("id_string type is no longer accepted", {
  expect_error(
    schema_validate(list(x = list(type = "id_string", default = "a"))),
    "\\$type must be one of"
  )
  expect_error(
    set_schema(vars = list(x = "id_string")),
    "\\$type must be one of"
  )
})

test_that("set_schema hybrid syntax accepts strings and lists, mixed", {
  s <- set_schema(vars = list(
    route_zone  = list(type = "categorical",
                       levels = c("urban", "suburban", "rural")),
    battery_pct = "percent",
    payload_kg  = list(type = "positive_numeric", max = 20),
    deliveries  = "count",
    prob_rain   = "probability"
  ))
  expect_equal(names(s), c("route_zone", "battery_pct", "payload_kg", "deliveries", "prob_rain"))
  expect_equal(s$battery_pct$type, "percent")
  expect_equal(s$payload_kg$max, 20)
  expect_equal(s$route_zone$levels, c("urban", "suburban", "rural"))
})

test_that("set_schema accepts named character vector shorthand", {
  s <- set_schema(vars = c(a = "count", b = "probability"))
  expect_equal(s$a$type, "count")
  expect_equal(s$b$type, "probability")
})

test_that("set_schema list spec without type errors", {
  expect_error(
    set_schema(vars = list(x = list(levels = c("a", "b")))),
    "`type` field",
    fixed = TRUE
  )
})

test_that("set_schema overwrite=FALSE errors on collision; TRUE replaces", {
  s0 <- set_schema(vars = list(x = "count"))
  expect_error(
    set_schema(vars = list(x = "probability"), schema = s0),
    "already exists"
  )
  s1 <- set_schema(vars = list(x = "probability"), schema = s0, overwrite = TRUE)
  expect_equal(s1$x$type, "probability")
})

test_that("set_schema remove drops vars and errors on unknown name", {
  s0 <- set_schema(vars = list(x = "count", y = "numeric"))
  s1 <- set_schema(schema = s0, remove = "x")
  expect_equal(names(s1), "y")
  expect_error(set_schema(schema = s0, remove = "nope"), "not found in schema")
})

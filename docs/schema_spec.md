# `fluxCore` Schema Specification

This document specifies the structure and meaning of a **entity schema** used by `fluxCore`. A schema defines the *core state variables* that a `Entity` can hold, along with defaults, coercion, validation, and optional block (panel) membership.

The schema is a **named list**. Each entry corresponds to one core state variable.

---

## 1. Schema object shape

A schema is a named list:

```r
schema <- list(
  var1 = <var_descriptor>,
  var2 = <var_descriptor>,
  ...
)
```

Each `<var_descriptor>` is itself a named list with a small vocabulary of recognized fields.

---

## 2. Variable descriptor fields

### 2.1 Required fields

#### `type`
**Type:** character scalar  
**Meaning:** Variable type label used by downstream summaries/validators.
Must be one of:

- `logical`
- `binary`
- `integer`
- `count`
- `nonnegative_integer`
- `positive_integer`
- `numeric`
- `nonnegative_numeric`
- `positive_numeric`
- `probability`
- `percent`
- `categorical`
- `ordinal`
- `string`
- `nonempty_string`

For backward compatibility, `continuous` is accepted and aliased to `numeric`.

Example:

```r
alive = list(type = "binary", default = TRUE, levels = c("0", "1"))
age = list(type = "nonnegative_integer", default = 0L)
risk = list(type = "probability", default = 0.5)
battery_pct = list(type = "percent", default = 100)
```

#### `default`
**Type:** scalar (length 1)  
**Meaning:** Default value for the variable at entity initialization when no `init` value is supplied.

Example:

```r
age = list(default = 40)
```

Notes:
- `default` may be `NA` (e.g., unknown at baseline).
- `default` should be a scalar; multi-length vectors are not supported as core state.

---

#### `levels`
**Type:** character vector  
**Meaning:** Allowed level labels for discrete state variables.

Required when `type` is one of `binary`, `categorical`, or `ordinal`.

Example:

```r
route_zone = list(
  type = "categorical",
  levels = c("urban", "suburban", "rural"),
  default = "urban"
)
```

---

### 2.2 Optional fields (recognized by fluxCore)

#### `coerce`
**Type:** function  
**Signature:** `function(x) -> scalar`  
**Meaning:** Applied to values entering the entity state, including:
- initialization via `Entity$new(init = ..., schema = ...)`
- all subsequent updates returned by `transition()`

Example:

```r
age = list(
  default = 40,
  coerce = as.numeric
)
```

Notes:
- Coercion should return a scalar (length 1).
- Coercion runs *before* validation (if validation is present).

---

#### `validate`
**Type:** function  
**Signature:** `function(x) -> TRUE/FALSE` (single logical scalar)  
**Meaning:** Predicate applied to coerced values. If it returns `FALSE`, entity initialization or the update errors.

Example:

```r
age = list(
  default = 40,
  coerce = as.numeric,
  validate = function(x) length(x) == 1L && is.finite(x) && x >= 0
)
```

Notes:
- Validation should be fast and deterministic.
- Validation should return a single TRUE/FALSE value, not a vector.

#### `allow_na`
**Type:** logical scalar  
**Default:** `FALSE`  
**Meaning:** If `TRUE`, missing values (`NA`) are accepted by built-in schema validation logic.

Example:

```r
bmp_order_time = list(
  default = NA_real_,
  coerce = as.numeric,
  allow_na = TRUE,
  validate = function(x) length(x) == 1L && (is.na(x) || is.finite(x))
)
```

#### `min` / `max`
**Type:** numeric scalar  
**Meaning:** Inclusive bounds enforced for numeric types after coercion.

Supported for types: `integer`, `count`, `nonnegative_integer`, `positive_integer`, `numeric`, `nonnegative_numeric`, `positive_numeric`, `probability`, `percent`.

Example:

```r
age = list(
  type = "nonnegative_integer",
  default = 40L,
  coerce = as.integer,
  min = 0L,
  max = 120L
)
risk = list(
  type = "probability",
  default = 0.5,
  coerce = as.numeric,
  min = 0,
  max = 1
)
```

#### `levels`
**Type:** character vector  
**Meaning:** Declares allowed values for discrete variables. For `binary`, `categorical`, and `ordinal` types, values are automatically validated against these levels when inserted into state.

Example:

```r
route_zone = list(
  type = "categorical",
  levels = c("urban", "suburban", "rural"),
  default = "urban",
  coerce = as.character,
  allow_na = FALSE
)
```

#### Built-in type validation

When no explicit `validate` function is provided, `fluxCore` applies built-in
validation based on the `type` field. Built-in validators are intentionally
**permissive within the type's semantic** — they enforce only what the type
name implies, not arbitrary range limits. Use `min` / `max` (numeric types)
or a custom `validate` function to tighten further.

- `logical`: Must be `TRUE` or `FALSE`.
- `binary`: Must be logical or match declared `levels`.
- `integer`: Must be an integer (whole number).
- `count`: Must be a non-negative integer.
- `nonnegative_integer`: Must be a non-negative integer.
- `positive_integer`: Must be a positive integer (>0).
- `numeric`: Must be numeric.
- `nonnegative_numeric`: Must be non-negative numeric.
- `positive_numeric`: Must be positive numeric (>0).
- `probability`: Must be numeric in [0,1].
- `percent`: Must be numeric in [0,100].
- `categorical`: Must match declared `levels`.
- `ordinal`: Must match declared `levels`.
- `string`: Must be character.
- `nonempty_string`: Must be non-empty character.

Example:

```r
age = list(type = "nonnegative_integer", default = 40L)
# Automatically validates >=0
risk = list(type = "probability", default = 0.5)
# Automatically validates 0 <= x <= 1
```

#### Validator presets
`fluxCore` provides helper constructors for common validation patterns:
- `schema_validator_numeric(min, max, allow_na = FALSE)`
- `schema_validator_integer(min, max, allow_na = FALSE)`
- `schema_validator_levels(levels, allow_na = FALSE)`

Example:

```r
statin_intensity = list(
  type = "categorical",
  levels = c("none", "moderate", "high"),
  default = "none",
  coerce = as.character,
  validate = schema_validator_levels(c("none", "moderate", "high"), allow_na = FALSE)
)
```

---

#### `required`
**Type:** logical scalar  
**Default:** `FALSE`  
**Meaning:** If `TRUE`, the variable must be supplied in `init` at entity creation, otherwise `Entity$new()` errors.

Example:

```r
sex = list(
  default = NA_character_,
  coerce = as.character,
  validate = function(x) length(x) == 1L && x %in% c("F","M"),
  required = TRUE
)
```

Use cases:
- demographic identifiers (age, sex)
- baseline disease markers required by the model

---

#### `blocks`
**Type:** character vector (or single string)  
**Meaning:** Defines block (panel) membership for vectorized updates, e.g., BP, BMP, lipids, medication sets.

Example:

```r
sbp = list(default = 120, coerce = as.numeric, blocks = "bp")
dbp = list(default = 80,  coerce = as.numeric, blocks = "bp")
```

Notes:
- A variable may belong to **multiple blocks**:

  (Example intuition: this is like one telemetry variable being reused by two
  operational groupings. In healthcare data, sodium often appears in both BMP and CMP panels.)

```r
sodium = list(default = 140, coerce = as.numeric, blocks = c("bmp", "cmp"))
```

- Block membership has **no ordering semantics**.
- Blocks are used by `update_block()` to validate and package updates. They do not change state behavior otherwise.

---

## 3. Field evaluation order

When a value is inserted into state (init or update), the intended order is:

1. **coerce** (if provided)
2. **validate** (if provided)
3. store value into core state

---

## 3.5 Building schemas with `set_schema()`

`set_schema()` builds (or extends) a validated schema using a hybrid `vars`
specification: each entry is either a **type-name string** or a **full list
spec**. Both shapes can be mixed in a single call.

```r
schema <- set_schema(vars = list(
  route_zone  = list(type = "categorical",
                     levels = c("urban", "suburban", "rural")),
  battery_pct = "percent",
  payload_kg  = list(type = "positive_numeric", max = 20),
  deliveries  = "count",
  prob_rain   = "probability"
))
```

**Signature:** `set_schema(vars = NULL, schema = NULL, remove = NULL, overwrite = FALSE)`

- `vars`: named list (or named character vector). Each element is either a
  single type-name string (e.g. `"count"`) or a list with `type` plus any
  recognized schema fields (`min`, `max`, `levels`, `default`, `coerce`,
  `validate`, `allow_na`, `required`, `blocks`).
- `schema`: optional existing schema to extend. If `NULL`, a new schema is
  created.
- `remove`: optional character vector of variable names to drop from `schema`
  before merging `vars`. Errors if any name is not present.
- `overwrite`: if `FALSE` (default), adding a variable already present in
  `schema` is an error. If `TRUE`, existing entries are replaced.

All produced specs flow through fluxCore's internal schema validator, so
type-driven defaults (`coerce`, `default`, validator) are filled in
automatically; user-supplied fields override the type defaults.

---

## 4. Recognized vs unrecognized fields

`fluxCore` recognizes the fields described above:  
`default`, `coerce`, `validate`, `required`, `blocks`.

Additional fields may appear in schema entries for model-level metadata (e.g., units, labels), but **fluxCore will ignore unrecognized fields** unless future versions explicitly adopt them.

If you add metadata fields, do not rely on fluxCore to enforce them.

---

## 5. Examples

### 5.1 Ordinal treatment (integer 0–4)

```r
n_antihypertensives = list(
  default = 0L,
  coerce = as.integer,
  validate = function(x) length(x) == 1L && !is.na(x) && x >= 0L && x <= 4L,
  blocks = "tx_htn"
)
```

### 5.2 Ordered category as character (simple, no factor storage)

```r
statin_intensity = list(
  default = "none",
  coerce = as.character,
  validate = function(x) length(x) == 1L && x %in% c("none","moderate","high"),
  blocks = "tx_lipid"
)
```

### 5.3 Lab order state

```r
bmp_order_time = list(
  default = NA_real_,
  coerce = as.numeric,
  validate = function(x) length(x) == 1L && (is.na(x) || is.finite(x))
)
```

---

## 6. Non-goals

The schema does not:
- encode derived variables (those are defined separately)
- encode event types or processes
- encode time units (declare canonical model time via bundle `time_spec(...)`)
- provide ordering-based mapping for vectorized models (use named outputs + blocks)

---

## 7. Practical guidance

- Prefer explicit `coerce` and `validate` for variables that matter in your domain.
- Use `required = TRUE` for variables your model cannot sensibly run without.
- Use `blocks` to support panel updates with `update_block(entity, block, values)` and `combine_updates(...)`.
- Keep core state scalar. If you need longitudinal arrays, represent them as events/observations instead.

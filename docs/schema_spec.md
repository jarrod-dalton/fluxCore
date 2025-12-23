# SCHEMA_SPEC.md
## patientSimCore schema specification

This document specifies the structure and meaning of a **patient schema** used by `patientSimCore`. A schema defines the *core state variables* that a `Patient` can hold, along with defaults, coercion, validation, and optional block (panel) membership.

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

### 2.1 Required field

#### `default`
**Type:** scalar (length 1)  
**Meaning:** Default value for the variable at patient initialization when no `init` value is supplied.

Example:

```r
age = list(default = 40)
```

Notes:
- `default` may be `NA` (e.g., unknown at baseline).
- `default` should be a scalar; multi-length vectors are not supported as core state.

---

### 2.2 Optional fields (recognized by patientSimCore)

#### `coerce`
**Type:** function  
**Signature:** `function(x) -> scalar`  
**Meaning:** Applied to values entering the patient state, including:
- initialization via `new_patient(init = ...)`
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
**Meaning:** Predicate applied to coerced values. If it returns `FALSE`, patient initialization or the update errors.

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

---

#### `required`
**Type:** logical scalar  
**Default:** `FALSE`  
**Meaning:** If `TRUE`, the variable must be supplied in `init` at patient creation, otherwise `new_patient()` errors.

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

## 4. Recognized vs unrecognized fields

`patientSimCore` recognizes the fields described above:  
`default`, `coerce`, `validate`, `required`, `blocks`.

Additional fields may appear in schema entries for model-level metadata (e.g., units, labels), but **patientSimCore will ignore unrecognized fields** unless future versions explicitly adopt them.

If you add metadata fields, do not rely on patientSimCore to enforce them.

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
- encode time units (use `ctx$time_unit` for that)
- provide ordering-based mapping for vectorized models (use named outputs + blocks)

---

## 7. Practical guidance

- Prefer explicit `coerce` and `validate` for variables that matter clinically.
- Use `required = TRUE` for variables your model cannot sensibly run without.
- Use `blocks` to support panel updates with `update_block(patient, block, values)` and `combine_updates(...)`.
- Keep core state scalar. If you need longitudinal arrays, represent them as events/observations instead.

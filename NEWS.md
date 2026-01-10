## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `patientSimCore`.

## patientSimCore 1.2.5

- Change: `ps_time_to_model()` now explicitly rejects time-only inputs (e.g., `difftime`, `hms`). Calendar inputs must be `Date` or `POSIXct` (date+time).
- Add: schema helper utilities for contract enforcement across the ecosystem: `ps_schema_validate()`, `ps_schema_assert_vars()`, `ps_schema_var_info()`, `ps_schema_assert_types()`, `ps_schema_assert_levels()`.
- Add: unit tests covering time-only rejection and schema helper behavior.

## 1.2.3

## patientSimCore 1.2.4

- Fix: unit tests updated to use `time_unit = ...` argument (avoid accidental partial match to `max_time`).
- Fix: add strict `max_time` validation to prevent silent mis-specified calls.
- Fix: `test-time-spec.R` string literals now use fixed matching (no invalid escapes).


- Add time-axis utilities for deterministic mapping between calendar time (Date/POSIXct) and numeric model time: `ps_time_spec()`, `ps_time_to_model()`, `ps_time_from_model()`, and `ps_set_time_unit()`.
- Change: time metadata is now stored under `ctx$time$unit` / `ctx$time$origin` / `ctx$time$zone` (replacing the older `ctx$time_unit` field).
- Notes: `months` and `years` are fixed approximations (30.4375 and 365.25 days). `origin` is a mapping reference, not model baseline.

## 1.2.2

- Fix: unit test for `derive(fn = "count", target = var(...))` now uses `snapshot_at_time()` (was incorrectly calling `snapshot_at()` with a time value).
- No behavior changes beyond test correction.

## 1.2.1

- Fix: `derive(fn = 'count', target = var(...))` now counts non-missing values in-window (avoids counting schema-default init placeholders such as `NA`).
- Add unit test locking this behavior.

## 1.2.0

- Add unit test covering `run_cohort(backend = 'cluster')` to reduce parallel-backend drift risk.

## patientSimCore 1.1.8

- Fix parse error in `batch.R` (remove stray parenthesis) affecting installation.

# 1.1.7 (2026-01-06)

- Fix: remove duplicated `id` argument in `Patient$initialize()` (package parse/collate error).

# 1.1.6 (2026-01-06)

- Added optional Patient$id field for user-supplied de-identified identifiers.
- Refactored cluster backend to avoid hard-coded clusterExport varlists by running worker logic from the package namespace.

# patientSimCore 1.1.4

## patientSimCore 1.1.5

- Fix: `run_cohort()` run index ordering is now `patient_id -> draw_id -> sim_id` (contractual invariant).

- Fix run-index ordering unit test to use a minimal bundle that always proposes a single no-op event (avoids Engine error when no proposals are available).
- Set `time_unit` in the ordering test to avoid warnings.

## 1.1.3
- Add explicit unit test enforcing run_cohort run-index ordering (patient_id → draw_id → sim_id) using a minimal bundle that assumes only core state vars.
- No behavior changes; strengthens contract guarantees for downstream packages.

# patientSimCore 1.1.1

- Add explicit unit test enforcing run index ordering (patient_id → draw_id → sim_id).
- No behavior changes; strengthens contract guarantees for downstream packages.

## patientSimCore 1.1.0

- Version bump (minor release).

## patientSimCore 1.0.14

- Update unit tests to explicitly declare non-core variables (age, miles_to_work, sbp, dbp) in schema.

## patientSimCore 1.0.13

- Fix unit test schema for sbp to include type metadata.

## patientSimCore 1.0.12

- Fix .validate_schema() to return the normalized schema (was incorrectly returning TRUE).

## patientSimCore 1.0.11

- Remove stray token in schema.R causing parse error.

## patientSimCore 1.0.10

- Fix syntax error (trailing comma) in default_patient_schema().

## patientSimCore 1.0.9

- Make default_patient_schema minimal (engine-level only).
- Update unit tests to explicitly declare non-core variables with type metadata.

## patientSimCore 1.0.8

- Add sbp to default schema and update tests for required schema typing.

## patientSimCore 1.0.7

- Fix missing brace in internal schema validator.

## patientSimCore 1.0.6

- Fix syntax error in schema levels quoting.
- Enforce schema typing (type + levels for binary/categorical/ordinal).

# patientSimCore 1.0.4
- Fix: `run_cohort()` now guarantees that `runs[[i]]` corresponds to `index[i, ]` (run_index alignment invariant). This removes the need for downstream reordering hacks and is critical for correct patient-level grouping.

# patientSimCore 1.0.3
- Add optional schema metadata fields `type` and `levels` (used by downstream summary code).
- Default schema now tags core variables with types (binary/continuous).

# patientSimCore 1.0.1


## patientSimCore 1.0.2
- No functional changes. Version bump to align with patientSimForecast 1.0.2.

- Clarified documentation around `active_followup`: it is a regular state variable and does not automatically stop the Engine.

# patientSimCore 1.0.0

- Stabilized `run_cohort()` context handling: `ctx` may be a single list (recycled) or a per-parameter-draw list-of-ctx (length = n_param_draws).
- `print.ps_state()` implemented to match the declared S3 method and remove the NAMESPACE warning.
- Namespace tightened: internal helpers remain unexported; core remains the sole owner of simulation state and execution semantics.

## fluxCore 1.7.0

- Coordinated ecosystem release alignment to version 1.7.0.
- Carries forward the canonical `time_spec` runtime contract introduced in
  1.5.1 (single model declaration via `bundle$time_spec`, no runtime override).

## fluxCore 1.5.1

- Canonical time-spec contract: simulation time is now declared once via
  `bundle$time_spec`; runtime context attempts to override time metadata now
  error.
- Removed runtime `time_unit` wiring from cohort/engine run APIs and aligned
  manual documentation with actual signatures.
- Added/updated unit tests to lock canonical time propagation and override
  protection behavior in serial and backend execution paths.

## fluxCore 1.5.0

- Finalized ecosystem harmonization for the 1.5.0 coordinated release.

- Documentation quality pass: fixed check() example/codoc/Rd usage issues and completed missing manual Rd coverage for exported APIs.

- run_cohort() hardening: unnamed entity lists are now auto-named for stable run indexing in examples and batch execution.

- Licensing update: switched package license to LGPL-3.

## fluxCore 1.4.0

- API naming: removed `flux_` prefixes from exported time/schema helpers. New names are `set_time_unit()`, `time_spec()`, `time_to_model()`, `time_from_model()`, `schema_validate()`, `schema_assert_vars()`, `schema_var_info()`, `schema_assert_types()`, and `schema_assert_levels()`.
- Documentation: manual `.Rd` pages were renamed/updated to match the new helper names.
- Packaging hygiene: standardized filenames to underscore style and retained manual documentation workflows (no roxygen generation).

## fluxCore 1.3.3

- Removed exported `var()` to avoid masking `stats::var()`. Use `declare_variable()` instead.
- Package documentation and NAMESPACE are maintained manually (no roxygen2).

# fluxCore 1.3.2

- Rename exported helper `var()` to `declare_variable()` to avoid masking `stats::var()` when attaching `fluxCore`.

## fluxCore 1.3.1

- Added `Entity$meta` for bundle/runtime bookkeeping (e.g., refresh cadence clocks) without polluting the validated state schema.

## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `fluxCore`.

## fluxCore 1.2.5

- Change: `time_to_model()` now explicitly rejects time-only inputs (e.g., `difftime`, `hms`). Calendar inputs must be `Date` or `POSIXct` (date+time).
- Add: schema helper utilities for contract enforcement across the ecosystem: `schema_validate()`, `schema_assert_vars()`, `schema_var_info()`, `schema_assert_types()`, `schema_assert_levels()`.
- Add: unit tests covering time-only rejection and schema helper behavior.

## 1.2.3

## fluxCore 1.2.4

- Fix: unit tests updated to use `time_unit = ...` argument (avoid accidental partial match to `max_time`).
- Fix: add strict `max_time` validation to prevent silent mis-specified calls.
- Fix: `test-time-spec.R` string literals now use fixed matching (no invalid escapes).


- Add time-axis utilities for deterministic mapping between calendar time (Date/POSIXct) and numeric model time: `time_spec()`, `time_to_model()`, `time_from_model()`, and `set_time_unit()`.
- Change: time metadata is now stored under `ctx$time$unit` / `ctx$time$origin` / `ctx$time$zone` (replacing the older `ctx$time_unit` field).
- Notes: `months` and `years` are fixed approximations (30.4375 and 365.25 days). `origin` is a mapping reference, not model baseline.

## 1.2.2

- Fix: unit test for `derive(fn = "count", target = declare_variable(...))` now uses `snapshot_at_time()` (was incorrectly calling `snapshot_at()` with a time value).
- No behavior changes beyond test correction.

## 1.2.1

- Fix: `derive(fn = 'count', target = declare_variable(...))` now counts non-missing values in-window (avoids counting schema-default init placeholders such as `NA`).
- Add unit test locking this behavior.

## 1.2.0

- Add unit test covering `run_cohort(backend = 'cluster')` to reduce parallel-backend drift risk.

## fluxCore 1.1.8

- Fix parse error in `batch.R` (remove stray parenthesis) affecting installation.

# 1.1.7 (2026-01-06)

- Fix: remove duplicated `id` argument in `Entity$initialize()` (package parse/collate error).

# 1.1.6 (2026-01-06)

- Added optional Entity$id field for user-supplied de-identified identifiers.
- Refactored cluster backend to avoid hard-coded clusterExport varlists by running worker logic from the package namespace.

# fluxCore 1.1.4

## fluxCore 1.1.5

- Fix: `run_cohort()` run index ordering is now `entity_id -> param_draw_id -> sim_id` (contractual invariant).

- Fix run-index ordering unit test to use a minimal bundle that always proposes a single no-op event (avoids Engine error when no proposals are available).
- Set `time_unit` in the ordering test to avoid warnings.

## 1.1.3
- Add explicit unit test enforcing run_cohort run-index ordering (entity_id → param_draw_id → sim_id) using a minimal bundle that assumes only core state vars.
- No behavior changes; strengthens contract guarantees for downstream packages.

# fluxCore 1.1.1

- Add explicit unit test enforcing run index ordering (entity_id → param_draw_id → sim_id).
- No behavior changes; strengthens contract guarantees for downstream packages.

## fluxCore 1.1.0

- Version bump (minor release).

## fluxCore 1.0.14

- Update unit tests to explicitly declare non-core variables (age, miles_to_work, sbp, dbp) in schema.

## fluxCore 1.0.13

- Fix unit test schema for sbp to include type metadata.

## fluxCore 1.0.12

- Fix .validate_schema() to return the normalized schema (was incorrectly returning TRUE).

## fluxCore 1.0.11

- Remove stray token in schema.R causing parse error.

## fluxCore 1.0.10

- Fix syntax error (trailing comma) in default_entity_schema().

## fluxCore 1.0.9

- Make default_entity_schema minimal (engine-level only).
- Update unit tests to explicitly declare non-core variables with type metadata.

## fluxCore 1.0.8

- Add sbp to default schema and update tests for required schema typing.

## fluxCore 1.0.7

- Fix missing brace in internal schema validator.

## fluxCore 1.0.6

- Fix syntax error in schema levels quoting.
- Enforce schema typing (type + levels for binary/categorical/ordinal).

# fluxCore 1.0.4
- Fix: `run_cohort()` now guarantees that `runs[[i]]` corresponds to `index[i, ]` (run_index alignment invariant). This removes the need for downstream reordering hacks and is critical for correct entity-level grouping.

# fluxCore 1.0.3
- Add optional schema metadata fields `type` and `levels` (used by downstream summary code).
- Default schema now tags core variables with types (binary/continuous).

# fluxCore 1.0.1


## fluxCore 1.0.2
- No functional changes. Version bump to align with fluxForecast 1.0.2.

- Clarified documentation around `active_followup`: it is a regular state variable and does not automatically stop the Engine.

# fluxCore 1.0.0

- Stabilized `run_cohort()` context handling: `ctx` may be a single list (recycled) or a per-parameter-draw list-of-ctx (length = n_param_draws).
- `print.flux_state()` implemented to match the declared S3 method and remove the NAMESPACE warning.
- Namespace tightened: internal helpers remain unexported; core remains the sole owner of simulation state and execution semantics.

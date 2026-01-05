# patientSimCore 1.1.2

- Fix new run-index ordering test to use default schema variables only.
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


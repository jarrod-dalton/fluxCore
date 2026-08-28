## fluxCore 2.1.0

This release repairs the lifecycle of policy-proposed actions. Actions are now held
in an engine-owned store, separate from the proposals a model manages through
`propose_events()` and `refresh_rules()`. Two defects followed from the previous
arrangement, in which both lived in one dictionary governed by the model's refresh
contract.

### Bug fixes

- **Cohort parameter contexts are no longer nested or renumbered.**
  `run_cohort(param_draws = )` and `bundle$sample_params(D)` now use one
  unambiguous `list<ParamContext>` boundary. Core validates and sorts the
  collection once by its positive, unique `draw_id`, preserves each context's
  direct `params` and `provenance` fields in callbacks, uses the actual ids in
  run indexing and deterministic seeds, and returns the canonical collection.
  Bare parameter payload lists at this Core boundary now fail early. When no
  draw source is supplied, Core constructs typed `1:D` contexts from
  `bundle$params` or an empty list.

- **Cohort and lower-level draw seeds are no longer overwritten by a loaded
  Engine.** `run_cohort()` now resolves its effective runtime settings once and
  owns coordinate-specific seeding; `Engine$run_draw()` preserves RNG state
  established by its caller. A private handoff prevents `Engine$run()` from
  applying the stored RuntimeContext seed a second time, while direct
  `Engine$run()` seeding is unchanged. Seeded results that previously collapsed
  distinct cohort or streaming replicates will change under the corrected
  ownership contract.

- **`ParamContext(draw_id = )` no longer truncates invalid ids.** Positive
  whole-valued doubles such as `5.0` retain the documented convenience and are
  stored as integers; fractional, non-positive, non-finite, and out-of-range
  values now error.

- **A scheduled action could be silently discarded before it fired.** Refreshing all
  processes -- the default when a bundle does not supply `refresh_rules()` -- replaced
  the whole proposal set, destroying any action proposed in an earlier step that had
  not yet been realized. An action scheduled meaningfully into the future would
  therefore never happen. Pending actions are now untouched by refresh, under either
  refresh strategy.

- **A realized action could repeat indefinitely.** When `refresh_rules()` returned a
  selective list of process ids, the action that had just been realized stayed in the
  proposal set and was selected again at the same instant, on every subsequent step.
  The engine now retires an action as soon as it is realized. This was not something a
  model could work around: the engine identified pending actions by an internal name
  the model was never given.

### New features

- **`propose_events()` may declare `last_event`.** When declared, it receives the event
  that was just realized, including an `ActionEvent`'s `params`, `metadata`, and
  `decision_point_id`. This makes it possible for a parameterized action to influence a
  future event process without a state variable used purely to carry the value.
  `last_event` is `NULL` on the first call, which also distinguishes initial proposal
  generation from a mid-run refresh -- previously indistinguishable. Callbacks that do
  not declare the argument are called exactly as before.

- **`DecisionPoint(on_pending_action = )`** declares what happens when a policy proposes
  an action while that decision point's previous action is still pending: `"warn"`
  (the default; supersede and warn), `"replace"` (supersede silently), `"keep"`
  (discard the new proposal), or `"error"`. A decision point re-proposing after its own
  action has fired is not a conflict and never warns.

- **`Engine$run()` reports `stopped_by`**, one of `"stop"`, `"max_time"`,
  `"max_events"`, or `"no_proposals"`. Previously a run that exhausted its event budget
  ended silently and was indistinguishable from normal completion.

### Behavior changes

- **Process ids beginning with `.` are reserved** for internal use and are rejected from
  both `propose_events()` and `refresh_rules()` with an explanatory error.

- **A realized action no longer carries a `process_id`.** A `process_id` identifies a
  model process, and an action is not one; actions are identified by
  `decision_point_id`, which `ActionEvent()` already carries. `is.null(event$process_id)`
  therefore distinguishes a policy action from a model event, and a model process may
  safely share a name with a decision point.

- **`load_model()` rejects duplicated `DecisionPoint` ids.** The engine keys pending
  actions by decision point id, so duplicates would silently collapse into one slot.

- **Several pending actions can now coexist and all will be realized.** Each decision
  point holds at most one pending action, but distinct decision points hold their own.
  Scheduling one action earlier than another orders them; it does not cancel the later
  one. Where two decision points represent alternative responses, make them mutually
  exclusive with `condition` rather than relying on `time_next` ordering. Tutorial 03
  has been updated accordingly.

## fluxCore 2.0.0

This is a major release. The core engine is unchanged; v2.0.0 layers a formalized
decision/policy/action architecture on top of it and replaces the old catch-all
`ctx` argument with explicit typed context objects.

### Decision points, actions, and trajectory logging

- **`DecisionPoint()`**: declares a named checkpoint in the event timeline where
  a policy can propose an action. Declared on the schema, not buried in transition
  logic. Supports `trigger` (which event types fire it), `allowed_actions`,
  `action_handlers` (per-action state-change functions), an optional `condition`
  predicate, and `audit` flag.
- **`ActionEvent`**: an action proposed by a policy enters the normal event
  timeline and is realized by the same `transition()` / `stop()` path as any
  other event. Actions do not mutate state directly.
- **`TrajectoryRecord`**: logged at every decision point firing. Records the time,
  decision point id, what state the policy observed, what it proposed, what was
  realized, and state before/after. Captures the full decision audit trail.
- **`trajectory_table()`**: convenience helper to flatten a list of
  `TrajectoryRecord` objects into a tidy data frame.
- **`load_model()`**: validated assembly function. Accepts `schema`, `bundle`,
  `policy`, `trajectory`, `runtime`, and `param_source`; validates that all
  components are mutually consistent and returns a configured `Engine`. This is
  now the recommended entry point for models with policies or runtime config.

### Typed context objects replacing `ctx`

`ctx` is removed as a first-class interface. Bundle callbacks that declared `ctx`
as a formal now receive a hard error on engine construction. Replace with the
following typed objects (all optional in callback signatures):

- **`SimContext`**: per-run metadata (`run_id`, `time_spec`, `model_id`,
  `scenario_id`, `horizon`).
- **`ParamContext`**: one parameter realization (`draw_id`, `params` named list,
  optional `provenance`). Constructed by `ParamContext()`.
- **`RuntimeContext`**: reproducibility and backend settings (`seed`,
  `replicate_id`, `backend`, `n_workers`). Constructed by `RuntimeContext()`.
- **`EnvironmentContext`**: external signals for ABM/RL hooks (`signals`,
  `step_fn`, `reset_fn`, `info`). Reserved for future use.

### Parameter uncertainty

- **`sample_params(n)` bundle hook**: when present, `run_cohort()` calls it to
  draw `n` `ParamContext` objects and runs every entity under every draw, fully
  crossing entities × parameter draws × stochastic replicates.
- **`run_cohort()` `param_draws` argument**: alternatively, pass a pre-built list
  of `ParamContext` objects directly.
- **`batch$param_draws`**: drawn contexts are returned alongside results for
  reproducibility.

### Other engine improvements

- **`refresh_rules(entity, last_event, changes)`**: bundle hook controlling which
  processes re-propose after each event. Returns `"ALL"` (default) or a character
  vector of `process_id`s. `entity` is the full post-transition state;
  `changes` is only the delta from the last `transition()` call.
- **`derive()`** / derived variables: schema variables can declare a `f` function
  of `(entity, j, t)` computed on read rather than stored. Supports time-aware
  lookups via `snapshot_at_time()`.
- **`schema_validate()` type-implied bounds**: default, min, and max values are
  now cross-checked against the range implied by the declared type (e.g.,
  `nonnegative_integer` must have default ≥ 0; `probability` must be in [0, 1]).
- **`set_schema()` gains `time_spec` and `decision_points` arguments**: assemble
  a complete schema including clock spec and decision points in one call.
- **`ModelProvider` / `PackageProvider` / `FileProvider` / `MLflowProvider`**:
  unexported. `Engine$new(provider=)` removed. `Engine$new(bundle=)` and
  `load_model()` are the only Engine construction paths.

## fluxCore 1.11.0

- Migrated documentation from manual `.Rd` files to inline roxygen2 comments.
  `man/` and `NAMESPACE` are now generated artifacts; do not edit by hand.
- No functional changes to the public API.

## fluxCore 1.10.2

- **`Engine$new(bundle = ...)` shortcut.** New `bundle` parameter to the `Engine` constructor accepts a `ModelBundle` directly, bypassing the `ModelProvider` machinery for in-memory / inline models. Equivalent to writing your own one-method provider, but without the boilerplate. The `provider = ...` path is unchanged and remains the right choice for packaged or pluggable models. Supplying both `bundle` and `provider` is an error.
- Tutorial 01 (`tutorials/01_core_engine_scaffold.Rmd`) updated to use `Engine$new(bundle = toy_bundle)` directly; `ModelProvider` is now a forward-pointer aside rather than a required first-encounter concept. Vignette title renamed to "Engine and ModelBundle scaffold".
- `man/Engine.Rd` updated with a Constructor section documenting the two construction paths.

## fluxCore 1.10.1

- **Removed `id_string` type** (no deprecation alias). Use `nonempty_string` or a custom `validate` function for identifier columns; the supported type list is now 14 entries.
- **Added `percent` type**: numeric in [0, 100], honors optional `min` / `max` overrides.
- **`set_schema()` rewrite** with hybrid `vars` syntax. Each `vars` entry is now either a type-name string (e.g. `"count"`) or a full list spec (e.g. `list(type = "positive_numeric", max = 20)`); both shapes can be mixed in one call. New `overwrite = FALSE` argument errors on collision when extending an existing `schema` (set `overwrite = TRUE` to replace). New `remove =` argument drops named entries (errors if absent). Removed the previous `replace =` and `add =` arguments.
- Built-in type validators are now explicitly documented as **permissive within type semantic** (no arbitrary range limits beyond what the type implies); use `min` / `max` or a custom `validate` function to tighten.
- Schema spec doc updated to reflect the new `set_schema()` shape, the `percent` type, and the removal of `id_string`.

## fluxCore 1.10.0

- **Expanded type system**: introduced 14 built-in variable types: `logical`, `binary`, `integer`, `count`, `nonnegative_integer`, `positive_integer`, `numeric`, `nonnegative_numeric`, `positive_numeric`, `probability`, `categorical`, `ordinal`, `string`, `nonempty_string`, `id_string`.
- **Automatic type-specific defaults**: `default` and `coerce` fields are now optional in schema specifications; fluxCore applies appropriate defaults (e.g., `as.numeric` for numeric types, `NA_real_` for numeric defaults).
- **New `set_schema()` helper**: simplified schema creation via named character vector of type mappings; reduces boilerplate for common use cases.
- **Backward compatibility**: "continuous" type now aliases to "numeric" for existing code migration.
- **Enhanced schema validation**: `.validate_schema()` now handles automatic defaults; stricter type checking and validation rules for new types.
- **Updated documentation**: expanded `schema_spec.md` with complete type reference; tutorial examples showcase both manual and helper-function workflows.

## fluxCore 1.10.0

- Removed implicit runtime defaults: deleted package-level `default_model_bundle()` and `default_entity_schema()` from runtime code.
- `PackageProvider` now requires explicit `registry` input; no hidden fallback bundle is injected.
- Constructor simplification: removed `new_entity()` wrapper and standardized on `Entity$new()`; wrapper input-normalization behavior is now handled directly in `Entity$initialize()`.
- Documentation cleanup: removed default-function man pages and updated provider/composition docs and README examples to use explicit, domain-neutral bundle/schema definitions.
- Test architecture cleanup: moved former default schema/bundle helpers into test fixtures (`tests/testthat/helper_fixtures.R`).

## fluxCore 1.8.1

- Hardened `refresh_rules()` engine boundary validation:
  - allows exactly `"ALL"` (scalar), or
  - character vector of unique, non-empty process ids.
- Added explicit fail-fast error messages for malformed refresh outputs.
- Added regression tests for missing hook default (`"ALL"`), valid targeted
  refresh, and malformed return structures.
- Updated manual docs to clarify optional `refresh_rules` behavior and strict
  return contract.

## fluxCore 1.8.0

- Hardened engine/model-bundle proposal contracts with clearer validation around
  event proposal structure and terminal-event metadata handling.
- Added regression tests covering lifecycle and proposal-contract behavior.
- Added README release/download badges.

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

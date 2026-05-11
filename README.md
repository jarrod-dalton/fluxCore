# fluxCore
[![Release](https://img.shields.io/github/v/release/jarrod-dalton/fluxCore?display_name=tag)](https://github.com/jarrod-dalton/fluxCore/releases)
[![Downloads](https://img.shields.io/github/downloads/jarrod-dalton/fluxCore/total)](https://github.com/jarrod-dalton/fluxCore/releases)
[![License: LGPL-3](https://img.shields.io/badge/license-LGPL--3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Language: R](https://img.shields.io/badge/language-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

A small set of R tools for building **entity-level simulation models** where things happen at **irregular times** (not every day, not every month), and where each event can change only a few state variables.

This package is meant to be a **foundation**. It does not contain a disease model. Instead, it gives you a clear way to:

- store an entity’s current variables (“state”)
- record a time-ordered list of events (what happened, when)
- advance time from one event to the next
- update only the variables that change at an event (sparse updates)
- optionally record extra outputs (costs, utilities, measurements)
- run many entities, optionally in parallel
- (optional) represent **parameter uncertainty** using global parameter draws shared across entities
- (optional) add interventions (“policies”) without rewriting your baseline model

If you can describe your model as “a sequence of events over time that change state variables”, this scaffold is a good fit.

---

## Key terms (plain-language definitions)

**Entity (currently implemented as `Entity`)**  
An object that holds (1) the current values of state variables and (2) a record of events over time.

**State**  
The set of variables that describe the entity *right now* and can influence what happens next (route zone, battery level, payload, dispatch mode, etc.).

**Event**  
Something that occurs at a particular time and may change the entity state (dispatch check, delivery completion, shift end, maintenance event, etc.).

**Event time**  
A numeric time value on a single global time axis shared across processes.

The Engine treats time as *unitless* math, but **your model should declare a unit** (e.g., days, months, years) so rates, cadences, and derived-variable lookbacks are interpretable and consistent.

**Event type**  
A label for what kind of event occurred (e.g., `"dispatch_check"`, `"delivery_completed"`, `"end_shift"`).

**State update (patch)**  
A named list of only the variables that change at an event. Variables not in the patch are unchanged.

Example patch: `list(battery_pct = 82, payload_kg = 3.1)`

**Observation (optional)**  
Information you want to record for analysis/reporting that does *not* affect future events (cost, utility, “was this an ED visit?”, etc.). Observations are separate from state updates on purpose.

**Episode (optional)**  
A short period with its own internal logic (e.g., a hospitalization) that returns a *summary* back to the main entity state, and optionally a detailed record (“artifact”) if you want it.

---

## What happens in a simulation run?

A single simulation run repeatedly does the following:

1. **Decide the next event and its time**  
   Based on the current state, the model determines what happens next and when it happens.

2. **Compute the state changes caused by that event**  
   Return a sparse update patch (or `NULL` if nothing changes).

3. **Record the event and apply the state changes**  
   The entity event log gets a new row; state variables are updated.

4. **Optionally record observations**  
   If you want to log costs or other outputs, compute and store them here.

5. **Stop or repeat**  
   Stop if the model says the simulation is finished (for example, shift end), otherwise go back to step 1.

That is the full conceptual loop.

---

## What you need to provide: a “ModelBundle”

In `fluxCore`, a model is represented by a **ModelBundle**: a named list of functions that define your simulation rules.

A bundle must provide:


- `propose_events(entity, ctx, ...)`  
  Returns one *proposed future event* per process, as a named list keyed by `process_id`.
  Each proposed event is a list that includes at least `time_next` and `event_type`.


- `transition(entity, event, ctx)`  
  Returns a sparse patch: a named list of state updates, or `NULL`.


- `stop(entity, event, ctx)`  
  Returns `TRUE` to stop the run, `FALSE` to continue.

A bundle may also provide:


- `observe(entity, event, ctx)`  
  Returns extra outputs you want to record (costs, utilities, measurements, etc.).

- `refresh_rules(entity, last_event, changes, ctx)`
  Controls which processes should regenerate their next proposal after an event. Return
  "ALL" to refresh all processes, or a character vector of `process_id`s.

- `sample_params(D)`  
  Returns a list of length `D` containing parameter draw objects. This is used for parameter uncertainty (see below). Components that do not use parameter draws can ignore this.

### What is `ctx`?
`ctx` is an optional “context” list passed into bundle functions. It can include:
- `time` / `time_spec` (internal/canonical time metadata populated by the engine)
- `param_draw_id` and `sim_id` (identifiers when running repeated simulations)
- `params` (a parameter draw, if you are doing parameter uncertainty)
- any other run-level inputs you want to pass through cleanly

Declare the model time axis once in the model bundle via
`time_spec(unit = "...")`. Runtime `ctx` must not override canonical time settings.

Bundles that do not need context can ignore it.

---

## Minimal example (single entity)

This uses a small urban delivery toy bundle so the example is self-contained.

```r
library(fluxCore)
set.seed(1)

schema <- list(
  route_zone = list(
    type = "categorical",
    levels = c("urban", "suburban", "rural"),
    default = "urban",
    coerce = as.character
  ),
  battery_pct = list(type = "continuous", default = 100, coerce = as.numeric),
  payload_kg = list(type = "continuous", default = 0, coerce = as.numeric)
)

toy_bundle <- list(
  time_spec = time_spec(unit = "hours"),
  event_catalog = c("dispatch_check", "delivery_completed", "end_shift"),
  terminal_events = "end_shift",
  propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    list(
      dispatch = list(time_next = entity$last_time + stats::rexp(1, rate = 0.8), event_type = "dispatch_check"),
      delivery = list(time_next = entity$last_time + stats::rexp(1, rate = 1.2), event_type = "delivery_completed"),
      end_shift = list(time_next = 8, event_type = "end_shift")
    )
  },
  transition = function(entity, event, ctx = NULL) {
    if (identical(event$event_type, "dispatch_check")) {
      return(list(payload_kg = max(0, stats::rlnorm(1, log(2), 0.3))))
    }
    if (identical(event$event_type, "delivery_completed")) {
      s <- entity$as_list(c("battery_pct", "payload_kg"))
      return(list(
        battery_pct = max(0, as.numeric(s$battery_pct) - stats::rexp(1, rate = 1 / 4)),
        payload_kg = max(0, as.numeric(s$payload_kg) - stats::rlnorm(1, log(1), 0.4))
      ))
    }
    list()
  },
  stop = function(entity, event, ctx = NULL) identical(event$event_type, "end_shift")
)

p <- Entity$new(
  init   = list(route_zone = "urban", battery_pct = 100, payload_kg = 0),
  schema = schema,
  entity_type = "courier",
  time0  = 0
)

eng <- Engine$new(
  provider = list(load = function(model_spec = NULL, ...) toy_bundle)
)

out <- eng$run(p, max_events = 50)

tail(out$events, 5)
out$entity$state(c("route_zone", "battery_pct", "payload_kg"))
```

### Trajectory output contract (v2 trajectory logger)

When using v2 assembly (`load_model(..., trajectory = ...)`), `Engine$run()` adds
`trajectory_records` to the returned list.

Current contract for `trajectory_records`:
- It is a list of plain named lists (JSON-serializable by default).
- One record is emitted per fired decision point.
- `state_before`/`state_after` follow `trajectory$detail`:
  - `none`: both are `NULL`
  - `summary`: both are summary lists from `summary_fn` (default `state_summary_default`)
  - `full`: both are full `entity$current` snapshots

Each trajectory record contains:
- `run_id`, `entity_id`, `t`, `decision_point_id`
- `observation`, `realized_event`
- `candidate_actions`, `proposed_actions`, `selected_action`
- `state_before`, `state_after`, `reward`

This output shape is the portability-facing surface for Stage 3 and is designed to
round-trip through JSON cleanly.



---

## Schema blocks and vectorized updates

For convenience when building multivariate models (e.g., battery/payload telemetry),
schema entries may include an optional `blocks` field (many-to-many). This lets you
refer to groups of variables by name:

```r
schema <- list(
  battery_pct = list(type = "continuous", default = 100, coerce = as.numeric, blocks = c("vehicle_status", "telemetry")),
  payload_kg = list(type = "continuous", default = 0, coerce = as.numeric, blocks = "vehicle_status")
)
# battery_pct appears in two blocks; payload_kg appears in one.

vehicle_vars <- block_vars(schema, "vehicle_status")   # c("battery_pct", "payload_kg")
```

Model `transition()` functions can generate vector-valued predictions and expand
them into per-variable updates with helpers:

```r
transition <- function(entity, event, ctx) {
  if (event$event_type != "dispatch_check") return(NULL)
  draw <- c(82, 3.1) # battery_pct, payload_kg
  set_vars(vehicle_vars, draw)
}
```
---

## Running many entities (batch simulation)

Use `run_cohort()` to run a list of entities. You can also run in parallel across entities.

```r
library(fluxCore)
set.seed(1)

schema <- list(
  route_zone = list(type = "categorical", levels = c("urban", "suburban", "rural"), default = "urban", coerce = as.character),
  battery_pct = list(type = "continuous", default = 100, coerce = as.numeric),
  payload_kg = list(type = "continuous", default = 0, coerce = as.numeric)
)

entities <- lapply(1:10, function(i) {
  Entity$new(
    init = list(
      route_zone = c("urban", "suburban", "rural")[((i - 1) %% 3) + 1],
      battery_pct = 100 - i,
      payload_kg = i %% 4
    ),
    schema = schema,
    entity_type = "courier",
    time0 = 0
  )
})
names(entities) <- paste0("id", seq_along(entities))

eng <- Engine$new(provider = list(load = function(model_spec = NULL, ...) toy_bundle))

batch <- run_cohort(
  engine = eng,
  entities = entities,
  n_param_draws = 1,
  n_sims = 1,
  max_events = 100,
  backend = "none",
  seed = 123
)

head(batch$index)
```

`batch$index` describes each run (entity × draw × sim), so your outputs are easy to identify.

---

## Parameter uncertainty (optional): global parameter draws

Sometimes fitted statistical models support drawing parameters from an approximate sampling distribution, for example using a covariance matrix for regression coefficients.

This package supports the common workflow:

- draw `D` parameter sets once (global draws)
- reuse those draws across all entities
- run `S` stochastic simulations per entity per draw

You control this via:

- `n_param_draws = D`
- `n_sims = S`

Where do the parameter draws come from?
- If your bundle provides `sample_params(D)`, `run_cohort()` will use it.
- If not, draws default to `NULL` (which is fine for models that do not have parameter draws).

Bundles/components that use parameter draws can look at `ctx$params`. Components that do not (e.g., some machine learning models) can ignore `ctx$params`.

---

## Policies and interventions (optional)

Often you want to compare:
- baseline model dynamics
- baseline + intervention/policy

The goal is to avoid duplicating baseline code.

Use `compose_bundles(baseline, policy)` where `policy` is another bundle-like object that can add or override behavior.

Most commonly, a policy adds extra state updates during `transition()`.

---

## Episodes (optional): detailed submodels without bloating the main state

Some events (e.g., hospitalization) may have rich internal dynamics that you may or may not want to record in detail.

A simple approach is:

- represent “hospitalization” as an event on the main timeline
- optionally run a submodel for the hospital stay
- return a **summary patch** to update main state (e.g., LOS, complications, post-discharge risk)
- optionally return an **artifact** (a detailed trace) stored outside the entity state

The core package does not force any one approach. The example model package demonstrates a practical pattern.

---

## Where to look in the code

- `R/Entity.R` : entity state, history, and event log
- `R/schema.R`  : defining entity variables (schema)
- `R/engine.R`  : running a single entity simulation
- `R/batch.R`   : running many entities (serial/parallel; draws/sims)
- `R/compose.R` : layering interventions/policies
- `R/providers.R`: loading bundles from package/files/MLflow (stub)

---

## What this package does not assume

- It does not assume a specific disease area.
- It does not assume daily time steps or fixed visit schedules.
- It does not require parameter uncertainty (you can ignore draws).
- It does not require episodes/submodels (you can keep everything at the top level).

---

## Development

`man/` and `NAMESPACE` are **generated** — do not edit them by hand.

To regenerate after changing roxygen comments in `R/`:

```r
roxygen2::roxygenise(".")
```

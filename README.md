# patientSimCore

A small set of R tools for building **patient-level simulation models** where things happen at **irregular times** (not every day, not every month), and where each event can change only a few patient variables.

This package is meant to be a **foundation**. It does not contain a disease model. Instead, it gives you a clear way to:

- store a patient’s current variables (“state”)
- record a time-ordered list of events (what happened, when)
- advance time from one event to the next
- update only the variables that change at an event (sparse updates)
- optionally record extra outputs (costs, utilities, measurements)
- run many patients, optionally in parallel
- (optional) represent **parameter uncertainty** using global parameter draws shared across patients
- (optional) add interventions (“policies”) without rewriting your baseline model

If you can describe your model as “a sequence of events over time that change patient variables”, this scaffold is a good fit.

---

## Key terms (plain-language definitions)

**Patient**  
An object that holds (1) the current values of patient variables and (2) a record of events over time.

**State**  
The set of variables that describe the patient *right now* and can influence what happens next (age, biomarker level, treatment status, comorbidity flags, etc.).

**Event**  
Something that occurs at a particular time and may change the patient’s state (clinic visit, adverse event, hospitalization, death, etc.).

**Event time**  
A numeric time value on a single global time axis shared across processes.

The Engine treats time as *unitless* math, but **your model should declare a unit** (e.g., days, months, years) so rates, cadences, and derived-variable lookbacks are interpretable and consistent.

**Event type**  
A label for what kind of event occurred (e.g., `"visit"`, `"AE_Y"`, `"hospital_admit"`, `"death"`).

**State update (patch)**  
A named list of only the variables that change at an event. Variables not in the patch are unchanged.

Example patch: `list(bmi = 29.1, tx_on = 1L)`

**Observation (optional)**  
Information you want to record for analysis/reporting that does *not* affect future events (cost, utility, “was this an ED visit?”, etc.). Observations are separate from state updates on purpose.

**Episode (optional)**  
A short period with its own internal logic (e.g., a hospitalization) that returns a *summary* back to the main patient state, and optionally a detailed record (“artifact”) if you want it.

---

## What happens in a simulation run?

A single simulation run repeatedly does the following:

1. **Decide the next event and its time**  
   Based on the current state, the model determines what happens next and when it happens.

2. **Compute the state changes caused by that event**  
   Return a sparse update patch (or `NULL` if nothing changes).

3. **Record the event and apply the state changes**  
   The patient’s event log gets a new row; state variables are updated.

4. **Optionally record observations**  
   If you want to log costs or other outputs, compute and store them here.

5. **Stop or repeat**  
   Stop if the model says the simulation is finished (often at death), otherwise go back to step 1.

That is the full conceptual loop.

---

## What you need to provide: a “ModelBundle”

In `patientSimCore`, a model is represented by a **ModelBundle**: a named list of functions that define your simulation rules.

A bundle must provide:


- `propose_events(patient, ctx, ...)`  
  Returns one *proposed future event* per process, as a named list keyed by `process_id`.
  Each proposed event is a list that includes at least `time_next` and `event_type`.


- `transition(patient, event, ctx)`  
  Returns a sparse patch: a named list of state updates, or `NULL`.


- `stop(patient, event, ctx)`  
  Returns `TRUE` to stop the run, `FALSE` to continue.

A bundle may also provide:


- `observe(patient, event, ctx)`  
  Returns extra outputs you want to record (costs, utilities, measurements, etc.).

- `refresh_rules(patient, last_event, changes, ctx)`
  Controls which processes should regenerate their next proposal after an event. Return
  "ALL" to refresh all processes, or a character vector of `process_id`s.

- `sample_params(D)`  
  Returns a list of length `D` containing parameter draw objects. This is used for parameter uncertainty (see below). Components that do not use parameter draws can ignore this.

### What is `ctx`?
`ctx` is an optional “context” list passed into bundle functions. It can include:
- `time_unit` (a single string, e.g., `"days"`, `"months"`, `"years"`). All event times are interpreted in this unit.
- `draw_id` and `sim_id` (identifiers when running repeated simulations)
- `params` (a parameter draw, if you are doing parameter uncertainty)
- any other run-level inputs you want to pass through cleanly

Bundles that do not need context can ignore it.

---

## Minimal example (single patient)

This uses the small toy bundle that ships with the package so you can see the mechanics.

```r
library(patientSimCore)
set.seed(1)

p <- Patient$new(
  init   = list(age = 55, miles_to_work = 10),
  schema = default_patient_schema(),
  time0  = 0
)

eng <- Engine$new(
  provider   = PackageProvider$new(),
  model_spec = list(name = "default")
)

out <- eng$run(p, max_events = 50, ctx = list(time_unit = "years"))

tail(out$events, 5)
out$patient$state(c("age", "miles_to_work"))
```



---

## Schema blocks and vectorized updates

For convenience when building multivariate models (e.g., SBP/DBP, CBC/CMP panels),
schema entries may include an optional `blocks` field (many-to-many). This lets you
refer to groups of variables by name:

```r
schema <- default_patient_schema()
# Example: schema$sbp$blocks <- c("bp")
#          schema$dbp$blocks <- c("bp")

bp_vars <- block_vars(schema, "bp")   # c("sbp", "dbp")
```

Model `transition()` functions can generate vector-valued predictions and expand
them into per-variable updates with helpers:

```r
transition <- function(patient, event, ctx) {
  if (event$event_type != "bp_check") return(NULL)
  draw <- c(120, 78)
  set_vars(bp_vars, draw)
}
```
---

## Running many patients (batch simulation)

Use `run_cohort()` to run a list of patients. You can also run in parallel across patients.

```r
library(patientSimCore)
set.seed(1)

eng <- Engine$new(provider = PackageProvider$new(), model_spec = list(name = "default"))

patients <- lapply(1:10, function(i) {
  Patient$new(init = list(age = 40 + i, miles_to_work = 8),
              schema = default_patient_schema(),
              time0 = 0)
})
names(patients) <- paste0("id", seq_along(patients))

batch <- run_cohort(
  engine = eng,
  patients = patients,
  n_param_draws = 1,
  n_sims = 1,
  max_events = 100,
  time_unit = "years",
  backend = "cluster",
  n_workers = 4,
  seed = 123
)

head(batch$index)
```

`batch$index` describes each run (patient × draw × sim), so your outputs are easy to identify.

---

## Parameter uncertainty (optional): global parameter draws

Sometimes fitted statistical models support drawing parameters from an approximate sampling distribution, for example using a covariance matrix for regression coefficients.

This package supports the common workflow:

- draw `D` parameter sets once (global draws)
- reuse those draws across all patients
- run `S` stochastic simulations per patient per draw

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
- optionally return an **artifact** (a detailed trace) stored outside the patient state

The core package does not force any one approach. The example model package demonstrates a practical pattern.

---

## Where to look in the code

- `R/Patient.R` : patient state, history, and event log
- `R/schema.R`  : defining patient variables (schema)
- `R/engine.R`  : running a single patient simulation
- `R/batch.R`   : running many patients (serial/parallel; draws/sims)
- `R/compose.R` : layering interventions/policies
- `R/bundles.R` : an example bundle (toy dynamics)
- `R/providers.R`: loading bundles from package/files/MLflow (stub)

---

## What this package does not assume

- It does not assume a specific disease area.
- It does not assume daily time steps or fixed visit schedules.
- It does not require parameter uncertainty (you can ignore draws).
- It does not require episodes/submodels (you can keep everything at the top level).

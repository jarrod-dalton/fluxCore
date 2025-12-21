# patientSimCore

`patientSimCore` is an R package for building **patient-level, event-driven simulation models**.  
It provides a clear and explicit structure for simulating longitudinal patient trajectories when outcomes evolve through **events over time**, rather than at fixed time steps.

This package is intended for **applied researchers and data scientists** working in health, medicine, or related domains who want more control and transparency than is offered by off-the-shelf simulation tools, without having to build a simulation engine from scratch.

`patientSimCore` is a **scaffold**, not a finished simulator. It supplies the machinery for running simulations, but leaves substantive modeling decisions firmly in the hands of the user.

---

## Installation

This package is under active development and should be installed from source.

```r
# from a local clone
devtools::install()

# or from GitHub (if applicable)
devtools::install_github("YOUR_ORG/patientSimCore")
```

Dependencies are intentionally minimal to keep the core lightweight and easy to reason about.

---

## Motivation

Many clinical and health services processes are best described by **irregular events**, not evenly spaced time steps. Examples include:

- clinic visits
- hospital admissions and discharges
- laboratory testing
- treatment initiation or discontinuation
- disease progression events

These processes operate on **different schedules**, but they interact. A hospitalization may alter follow-up frequency; a lab result may trigger treatment; treatment may change the probability of future events.

Traditional discrete-time simulations often force these dynamics onto a fixed grid. `patientSimCore` instead models what actually happens: **events occur when they occur**, and patient state changes only when something happens.

---

## Mental model

A useful way to think about a simulation in `patientSimCore` is:

> “I have a patient whose state changes over time. Multiple processes can suggest future events. At each step, the earliest event happens, the patient changes state, and the future updates accordingly.”

Three ideas underpin the design:

1. **Patients are stateful objects**
2. **Events drive change**
3. **Time advances only when events occur**

---

## Core components

### Patients

Each patient is represented as an R6 object with:

- a user-defined state schema
- a complete event log
- a sparse history of state changes
- helper methods to recover state at any event or time

State is **event-sourced**. When an event occurs, only the variables that actually change are recorded. Full state at any point can always be reconstructed from the event history.

This approach keeps simulations transparent and avoids silently mutating hidden state.

---

### Engine

The engine runs the simulation loop. At a high level it:

1. asks one or more processes to propose future events
2. selects the next event on a shared time axis
3. applies the corresponding state transition
4. checks whether the simulation should stop

All processes share a **single global time axis**. Processes are not competing risks in the statistical sense; they are parallel event streams that interact through patient state.

---

### Model bundles

Model behavior is defined using a **ModelBundle**, which is a named list of functions. At minimum, a bundle provides:

- `propose_events(patient, ctx)`  
  Returns one or more candidate future events.

- `transition(patient, event, ctx)`  
  Applies state changes implied by the event.

- `stop(patient, event, ctx)`  
  Determines whether the simulation should terminate.

An optional `observe()` function can be used to record outputs or side effects.

Events themselves are passed intact through the system. The engine does not strip them down to event types or times.

---

## Quick start (toy example)

The following example illustrates the basic flow using a deliberately simple model.

```r
library(patientSimCore)

# Define a simple model bundle
toy_model <- list(

  propose_events = function(patient, ctx) {
    list(
      list(
        time = patient$time + rexp(1, rate = 0.2),
        event_type = "visit",
        process_id = "visits"
      )
    )
  },

  transition = function(patient, event, ctx) {
    if (event$event_type == "visit") {
      list(
        visits = patient$state()$visits + 1
      )
    } else {
      NULL
    }
  },

  stop = function(patient, event, ctx) {
    patient$state()$visits >= 5
  }
)

# Create a patient
patient <- Patient$new(
  schema = default_patient_schema(
    visits = 0
  )
)

# Run the engine
engine <- Engine$new(model = toy_model)
engine$run(patient)

# Inspect results
patient$state()
patient$event_log
```

This example demonstrates several key ideas:

- events are proposed into the future
- only changed state variables are recorded
- stopping logic is explicit and model-defined

---

## Key features

### Multiple event streams

Models can define multiple processes (e.g., visits, labs, hospitalizations), each proposing its own events. The engine always selects the earliest event across all processes.

This makes it natural to represent processes that operate on different clocks but interact through patient state.

---

### Refresh rules

Processes can specify when they need to regenerate their next proposed event. By default, a process regenerates after any state change, but more selective rules are supported.

This avoids unnecessary recomputation while keeping model logic explicit.

---

### Derived variables

Derived variables are computed **at snapshot time** and are not part of core patient state. This supports:

- lags
- rolling summaries
- lookback-based features

without contaminating the simulation state itself.

---

### Time-based access

The patient object supports:

- state at a specific event index
- state at a specific time
- full snapshots including derived variables

State at time `t` is defined as the most recent event time less than or equal to `t`.

---

### Batch simulation and uncertainty

Cohorts of patients can be simulated in batches. Parameter uncertainty can be handled at the model or provider level, with shared draws reused across patients when appropriate.

This supports replication, sensitivity analysis, and uncertainty propagation.

---

## Providers

Model bundles can be loaded via providers, including:

- package-based providers
- file-based providers

Providers may also supply parameter draws. This separation keeps model definition decoupled from how models and parameters are sourced.

---

## Worked example

A fully worked, end-to-end example is provided in the companion package:

**`patientSimModelExample`**

That package demonstrates:

- realistic model structure
- multiple interacting event streams
- parameter handling
- interpretation of simulation outputs

If you are new to `patientSimCore`, start there.

---

## Design philosophy

- Explicit over implicit
- Events over time grids
- Clarity over cleverness
- Scaffolding rather than black boxes

The goal is to make complex longitudinal simulations easier to reason about, inspect, and extend.

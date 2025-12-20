# A Conceptual Guide for Clinician–Modeler Collaboration in Patient-Level Simulation Studies

## Overview and scope

This document provides a conceptual framework for clinicians collaborating with quantitative modelers on the development of patient-level simulation models. It is intended for clinician–scientists who are actively involved in shaping model structure, assumptions, and interpretation, rather than serving solely as downstream consumers of model outputs.

The focus is on event-driven simulation models that represent individual patients over time. These models are increasingly used in health services research, clinical epidemiology, and decision science to study complex processes such as care pathways, policy interventions, and long-term outcomes under uncertainty.

The purpose of this guide is not to teach programming or specific software tools. Instead, it aims to clarify the conceptual responsibilities of clinicians in simulation model development, to describe the core abstractions used in these models, and to establish a shared language that facilitates effective interdisciplinary collaboration.

## Why patient-level simulation models are different

Many clinicians are familiar with statistical models that estimate associations or predict outcomes from observed data. Patient-level simulation models serve a different purpose.

Rather than estimating parameters from data alone, these models encode hypotheses about how clinical processes unfold over time. They allow investigators to explore counterfactual scenarios, evaluate policies that cannot be directly observed, and integrate multiple sources of evidence into a coherent dynamic representation.

Key distinguishing features include:

- explicit representation of time as an ordered sequence of events
- modeling of individual trajectories rather than aggregate averages
- separation between what happens (events) and what changes (state)
- explicit stopping conditions that define the scope of the simulated process

Because of these features, simulation models require more up-front conceptual specification than many statistical analyses. This is where clinical leadership is essential.

## The patient as the fundamental unit

At the core of these models is the individual patient. Each simulated patient is treated as a distinct entity whose clinical course unfolds over time.

### Patient state

The state of a patient refers to the collection of attributes that describe the patient at a particular point in the simulation. These attributes may include demographic characteristics, clinical variables, treatment status, risk indicators, or summary measures derived from prior history.

Important properties of patient state in this framework include:

- The state is always well-defined at each event.
- Not all state variables change at every event.
- State variables may be persistent (e.g., sex) or evolving (e.g., age, treatment exposure).

Clinicians play a central role in determining which aspects of patient state are represented. This involves decisions about clinical relevance, level of abstraction, and how much detail is necessary for the research question at hand.

## Events as the drivers of change

### Definition of an event

An event is a discrete occurrence at a specific time. Events represent the moments when something happens to or for the patient.

Examples include outpatient visits, hospital admissions, medication changes, adverse events, diagnostic tests, or death.

Each event is characterized by:

- a time at which it occurs
- a type or label indicating what kind of event it is
- an optional set of changes to patient state

### Events versus state updates

A key design principle is the separation between events and state updates. An event may occur without changing the patient’s state, and a state change always occurs in the context of an event.

This distinction allows the model to represent clinically meaningful situations such as visits with no treatment change, or observation-only events used for measurement and logging.

## Representation of time

Time in these models is represented as a numeric variable that increases monotonically as events occur. Time advances only when an event is recorded.

This reflects several realities of clinical care:

- encounters are irregularly spaced
- clinically meaningful changes often occur at discrete moments
- long intervals may pass without any relevant events

A critical invariant of the model is that event times must be non-decreasing. Violations of this assumption undermine causal interpretation and should be treated as modeling errors rather than tolerated behavior.

## The simulation process

At a high level, a simulation consists of repeatedly applying a small number of steps to a patient until a stopping condition is met.

1. Based on the patient’s current state, determine the next event and its timing.
2. Compute any changes to patient state associated with that event.
3. Record the event and apply the state updates.
4. Evaluate stopping conditions.
5. If stopping conditions are not met, repeat.

Although implemented computationally, each step corresponds to conceptual decisions about clinical processes.

## Event generation and transition logic

### Event generation

The event generation component determines what happens next in the simulation. It encodes assumptions about:

- which events are possible from a given state
- how long until the next event
- how treatments or policies influence event rates

Clinicians are responsible for ensuring that the space of possible events and their dependencies on patient state are clinically plausible.

### State transitions

State transition logic specifies how patient attributes change when an event occurs. Transitions may be deterministic or stochastic and may depend on multiple aspects of patient history.

Only variables that truly change should be updated at a given event. This sparse-update principle helps preserve interpretability and reduces unintended interactions.

## Policies and interventions

Policies are represented as modifications to event generation or state transition rules.

Examples include:

- selection between alternative treatments
- changes in monitoring intensity
- altered thresholds for escalation of care

Policies influence outcomes indirectly by shaping the sequence and nature of events experienced by patients. This structure mirrors real-world clinical systems and supports causal reasoning.

## Adverse events and downstream consequences

Clinical interventions often produce downstream consequences that unfold over multiple steps.

For example, a treatment may increase the risk of an adverse event, which in turn triggers additional encounters, investigations, or hospitalizations. Event-driven simulation models naturally accommodate such cascades by explicitly representing each link as an event.

Clinicians should carefully consider which adverse events are modeled, how they are detected, and what actions they precipitate.

## Stopping conditions and scope

Simulations must include explicit stopping rules that define the modeled scope.

Common stopping conditions include death, reaching a fixed time horizon, or entry into a terminal health state. The appropriate stopping rule depends on the research question and should be determined collaboratively.

Explicit stopping logic clarifies interpretation and prevents inadvertent extrapolation beyond the intended domain.

## Outputs and interpretation

Simulation outputs typically include complete event histories and state trajectories. While summary statistics are often reported, examining individual trajectories during development is essential.

Clinicians should evaluate whether simulated patient histories align with clinical experience and expectations, paying particular attention to implausible sequences or timing.

## Common pitfalls

Common modeling issues include:

- violations of temporal ordering
- state changes without clear clinical cause
- unrealistic event frequencies
- assumptions embedded implicitly rather than explicitly discussed

Active clinician involvement throughout development helps identify and correct these issues early.

## Collaboration as an iterative process

Simulation model development is inherently iterative. Initial versions are refined through repeated cycles of review, revision, and validation.

Effective collaboration requires shared vocabulary, frequent communication, and willingness to revisit assumptions as understanding evolves.

## Methods primer (journal-style conceptual framing)

### Conceptual model structure

The simulation model represents individual patients as stateful entities evolving through a sequence of discrete events over continuous time. Patient state is defined by a set of clinically meaningful attributes, and events are defined as discrete occurrences that may alter this state.

### Event dynamics

At each step of the simulation, a stochastic event generation mechanism proposes the next event type and timing conditional on the current patient state. This mechanism may incorporate fitted statistical models, externally estimated parameters, or rule-based logic informed by clinical expertise.

### State transitions

Following each event, a transition function updates a subset of patient state variables. These transitions may depend on patient history, current treatments, or external parameters, and are designed to reflect clinically plausible mechanisms.

### Stopping rules

The simulation terminates when a predefined stopping condition is met. Stopping conditions are explicitly specified and may include death, completion of a care episode, or attainment of a fixed time horizon.

### Outputs

The model produces complete patient-level event histories and state trajectories, from which outcomes of interest are derived.

## Appendix A: Glossary

**Event**  
A discrete occurrence at a specific time in the simulation (e.g., visit, hospitalization, death).

**State**  
The collection of patient attributes describing the patient at a given event.

**State transition**  
The change in one or more state variables resulting from an event.

**Event-driven simulation**  
A simulation framework in which time advances only at discrete events.

**Policy**  
A rule or intervention that alters event generation or state transitions.

**Stopping condition**  
A rule specifying when a simulation ends.

**Trajectory**  
The sequence of events and states experienced by a simulated patient.

**Sparse update**  
A modeling approach in which only variables that change are updated at an event.

**Counterfactual scenario**  
A hypothetical situation representing an alternative policy or intervention.

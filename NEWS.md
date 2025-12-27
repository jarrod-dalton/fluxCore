# patientSimCore 0.11.0

- Stabilized `run_cohort()` context handling: `ctx` may be a single list (recycled) or a per-parameter-draw list-of-ctx (length = n_param_draws).
- `print.ps_state()` implemented to match the declared S3 method and remove the NAMESPACE warning.
- Namespace tightened: internal helpers remain unexported; core remains the sole owner of simulation state and execution semantics.

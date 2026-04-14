# splitGraph 0.1.0

- Initial public release of `splitGraph`.
- Provides typed dependency-graph construction, validation, querying, and
  split-constraint derivation for leakage-aware evaluation workflows.
- Includes translation of constraints into a stable, tool-agnostic
  sample-level split specification (`split_spec`), plus preflight validation
  (`validate_split_spec()`) and leakage-risk summaries.
- `splitGraph` emits `split_spec` objects without runtime dependencies on
  downstream tooling. Adapters for specific consumers (bioLeak, fastml,
  rsample, ...) live in those packages so splitGraph remains neutral.

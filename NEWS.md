# splitGraph 0.1.0

Initial public release. Highlights:

- Typed dependency-graph construction from canonical metadata via
  `graph_from_metadata()`, or from explicit node and edge tables via
  `create_nodes()` / `create_edges()` / `build_dependency_graph()`.
- Structural, semantic, and leakage-relevant validation
  (`validate_graph()`), typed query helpers, and projected
  sample-dependency detection.
- Split-constraint derivation (`derive_split_constraints()`) for
  `subject`, `batch`, `study`, `time`, and `composite` modes, translated
  into a stable, tool-agnostic `split_spec` via `as_split_spec()` with
  preflight validation and leakage-risk summaries.
- `plot()` method for `dependency_graph` with a typed, layered layout,
  per-type node colors, and an auto-generated node-type legend.
- `splitGraph` emits `split_spec` objects without runtime dependencies on
  downstream tooling. Adapters are expected to live in consumer packages
  (e.g. packages built on top of `rsample`) so that `splitGraph` itself
  stays neutral.

See the README for a full feature overview and a runnable Quick Start.

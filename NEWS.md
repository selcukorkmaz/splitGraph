# splitGraph 0.3.0.9000 (development)

## New features

- **`Site` node type and `sample_collected_at_site` edge.** Multi-site / 
  multi-center structure is now a first-class typed relation.
  `graph_from_metadata()` auto-detects a `site_id` column; `validate_graph()`
  flags samples assigned to multiple sites (`sample_multiple_site_assignments`);
  `derive_split_constraints(mode = "site")` groups samples by collection site
  (and rejects ambiguous multi-site assignments); and `as_split_spec()` carries
  `site_group` as a blocking annotation in `sample_data`. `Site` also
  participates in `mode = "composite"` derivations and the typed `plot()` layout.
- **`Region` node type and `sample_located_in_region` edge.** Categorical tissue
  / anatomical region structure is now a first-class typed relation, mirroring
  `Site`: `region_id` auto-detection, multi-region validation
  (`sample_multiple_region_assignments`), `derive_split_constraints(mode = "region")`,
  a `region_group` blocking annotation in `split_spec`, and composite/plot support.
- **`Platform` node type and `sample_run_on_platform` edge, plus `platform`
  and `assay` constraint modes.** Sequencing / measurement-platform structure
  is now a first-class typed relation: `graph_from_metadata()` auto-detects a
  `platform_id` column, `validate_graph()` flags samples run on multiple
  platforms (`sample_multiple_platform_assignments`), and
  `derive_split_constraints(mode = "platform")` groups samples by platform.
  `mode = "assay"` reuses the existing `sample_measured_by_assay` relation to
  group by assay / modality. Both carry a blocking annotation in `split_spec`
  (`platform_group`, `assay_group`), participate in `mode = "composite"`
  (`via = c("Subject", "Platform")`), and are covered by the typed `plot()`
  layout. The `assay_uses_platform` edge (`Assay` -> `Platform`) is also part of
  the schema for manually encoding which platform an assay runs on.
- **Pairwise (thresholded) leakage relations: `relatedness` and `spatial`
  modes.** Some leakage sources are pairwise and continuous rather than clean
  groups. Two new undirected, thresholded edge types model them —
  `subject_related_to` (genetic relatedness between subjects) and
  `sample_adjacent_to` (spatial proximity between samples) — together with the
  edge-building helpers `relatedness_edges_from_kinship(pairs, threshold)` and
  `spatial_edges_from_coords(coords, radius)`.
  `derive_split_constraints(mode = "relatedness")` and `mode = "spatial"` then
  form groups as connected components (transitive closure) over the thresholded
  edges, so a chain of individually near neighbours still lands in one group.
  This is a grouping that column-based approaches structurally cannot express.
  Both modes honor the `samples=` subset (components are recomputed within the
  subset, so an excluded bridge sample cannot leak structure across the split).

# splitGraph 0.2.0

## New features

- **JSON serialization** for both core handoff objects:
  `write_dependency_graph()` / `read_dependency_graph()` and
  `write_split_spec()` / `read_split_spec()`. The on-disk format is
  documented under `?write_dependency_graph` and `?write_split_spec`,
  carries a `schema_version` field, and round-trips `NA` values
  faithfully. This makes `splitGraph` objects portable across R sessions
  and across language boundaries (Python, Julia, CLI tools — anything
  that can read JSON). Requires `jsonlite` (added to `Suggests`); a
  helpful error is raised if it is not installed.
- A new vignette, `adapter-cookbook`, walks through three small adapter
  patterns: a base-R leave-one-group-out adapter (executed), a
  grouped-CV adapter built on `rsample::group_vfold_cv()`
  (illustrative), and an ordered-evaluation adapter built on
  `rsample::rolling_origin()` (illustrative). It also shows the JSON
  cross-language handoff path.
- `validate_graph()` and `validate_depgraph()` now accept a documented
  `validation_overrides` argument. Currently supported override:
  `allow_multi_subject_samples` (default `FALSE`); when `TRUE`, samples
  linked to multiple subjects are not flagged by the semantic validator and
  `derive_split_constraints(mode = "subject")` keeps the first listed
  assignment instead of erroring. The override mechanism is now first-class
  and merges into any graph-level override for the duration of the call
  only.
- `derive_split_constraints(mode = "subject")` honors
  `allow_multi_subject_samples` consistently with `validate_graph()`. When
  the override is active and ambiguity exists, the choice is recorded in
  `metadata$warnings` so it is not invisible.
- `query_paths()` now applies a finite default safety cap on `max_length`
  (`8` edges) so that `igraph::all_simple_paths()` cannot blow up on dense
  graphs. Pass `max_length = Inf` to opt out and search exhaustively, or
  any non-negative integer for an explicit cap. Negative values and
  non-numeric inputs are rejected with a clear error.

## Improvements

- `build_dependency_graph()` returns a more useful error when edge
  endpoints don't match any node ID and the mismatch looks like a prefix
  problem (e.g. edges built with `from_prefix = TRUE` but nodes built with
  `prefix = FALSE`). The error now points at the likely cause.
- `graph_from_metadata()` warns when it constructs `Outcome` nodes from a
  numeric `outcome_value` column (which produces `outcome:0` / `outcome:1`
  nodes that are usually not what the user wants). To silence the warning,
  pass `outcome_id` (character class label) instead, or coerce
  `outcome_value` to character first.
- The data-model `schema_version` is now explicitly decoupled from the
  package version. Bumping the package will not bump the schema; only an
  explicit, documented schema change should.

## Deprecations

These remain functional in 0.2.0 but emit `.Deprecated()` warnings and are
scheduled for removal in a future release. Use the canonical names instead:

- `validate_graph(checks = …)` → use `levels = …` and `severities = …`.
- `build_depgraph()` → use `build_dependency_graph()`.
- `validate_depgraph()` → use `validate_graph()`.
- `new_depgraph_nodes()` → use `graph_node_set()`.
- `new_depgraph_edges()` → use `graph_edge_set()`.
- `new_depgraph()` → use `dependency_graph()`.

## Documentation

- README and the main vignette now use `outcome_id` (character) in their
  quick-start example instead of numeric `outcome_value`, matching the new
  warning behavior.
- The `validation_overrides` parameter and the `allow_multi_subject_samples`
  key are now documented on `?build_dependency_graph` /
  `?validate_graph`.

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

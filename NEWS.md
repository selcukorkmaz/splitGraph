# splitGraph 0.4.0

A deliberate **breaking release** (see "Breaking changes" below); everything
after it is intended to be additive. The theme is maturity: every step of the
pipeline is now linear in the size of the dataset, the `split_spec` contract is
self-contained and schema-checked, the 0.1-era API debt is gone, and what each
downstream consumer actually accepts is documented and pinned by tests.

## Performance

Every step of the pipeline is now linear in the number of nodes and edges, and
the derivation results, validation issues, and written JSON are byte-identical
to 0.3.0 on regression cohorts.

- **Composite and pairwise grouping no longer enumerate sample pairs.**
  `derive_split_constraints(mode = "composite")`, `mode = "relatedness"`,
  `mode = "spatial"`, and `detect_dependency_components()` compute connected
  components on the bipartite sample-target graph. With the default `via`
  (subject, batch, study, time) composite derivation of 500 samples took about
  70 s and grew quadratically (about 3.5 min at 750 samples); it now takes
  under 0.1 s at 5 000 samples and about 0.2 s at 20 000.
- **Direct assignments, sample maps, and rule-based composites are vectorised**
  (one `data.frame()` per table instead of one per sample).
  `as_split_spec(constraint, graph = g)` dropped from 27 s to about 0.3 s at
  5 000 samples.
- **Validation builds its issue table once** instead of `rbind`-ing one-row
  frames, checks edge signatures with a single join against the schema, and
  replaces `stats::aggregate()` with `split()`-based counting: 7 s to under 1 s
  at 5 000 samples.
- **JSON writers hand jsonlite data frames** instead of nested per-row lists,
  and `spatial_edges_from_coords()` scans the distance matrix with `which()`
  instead of a double loop.
- `create_nodes()` detects conflicting duplicate definitions in one pass over
  the distinct rows rather than one table scan per duplicated identifier.
- New `inst/bench/pipeline.R` reproduces the timings, and
  `tests/testthat/test-performance.R` (skipped on CRAN) fails if any step
  exceeds a generous wall-clock budget or composite derivation stops scaling
  linearly.

## New features

**`split_spec` contract (schema 0.3.0)**

- **Stratum annotation.** `as_split_spec(constraint, graph = g)` fills a new
  `stratum` column with the key of the single `Outcome` node attached to each
  sample (`sample_has_outcome`, falling back to the subject's single outcome via
  `subject_has_outcome`) and declares it through `stratum_var`. It is an
  annotation of the outcome level, exposed so consumers such as scikit-learn's
  `StratifiedGroupKFold` can stratify; splitGraph still never balances folds.
  `validate_split_spec()` checks the declared column (`invalid_stratum_var`,
  `empty_stratum_var`, `partial_stratum`). The Python reader gains
  `stratum_var`, a default for `strata()`, and `stratified_group_kfold()`; the
  conformance test compares strata as well as grouping and ordering.
- **Pairwise sources inside composite derivations.** `via` for
  `mode = "composite"` now accepts `"relatedness"` and `"spatial"` (or their
  relation names) alongside the direct sources. In the strict strategy their
  thresholded edges join the same connected-component search; in the
  rule-based strategy they contribute the component label, and a singleton
  component counts as no assignment so the sample falls through to the next
  mode. `metadata$via` lists node types for direct sources and mode names for
  pairwise ones.
- **Provenance.** `relatedness_edges_from_kinship()` and
  `spatial_edges_from_coords()` record their threshold and metric on the edge
  set's `source`; `build_dependency_graph()` carries every edge set's source
  into `metadata$edge_sources` (also serialised); pairwise constraints report
  `metadata$threshold` / `threshold_metric`; and `as_split_spec()` adds `via`,
  `priority`, `threshold`, `threshold_metric`, and `igraph_version` to the spec
  metadata.
- **Schema 0.3.0, versioned.** `schema_version` moves to `"0.3.0"` (same major:
  `0.1.0` and `0.2.0` files still load silently) and the shipped schemas now
  live under `inst/schema/<version>/`, so a file's `$schema` URL stays valid
  after later bumps. `migrate_split_spec_json()` fills `stratum` with `NA`.
- **Validating readers.** `read_dependency_graph(path, validate = TRUE)` and
  `read_split_spec(path, validate = TRUE)` check the file against the shipped
  schema before parsing and run `validate_graph()` / `validate_split_spec()`
  on the result, failing with a classed error. The R-side JSON validators
  themselves now check that `attrs` is an object, that `sample_data` columns
  have the declared types, and that vector-valued metadata fields are arrays.
- **New leakage rule `subject_cross_site_overlap`** (warning): a subject with
  samples collected at several sites, mirroring `subject_cross_study_overlap`;
  `summarize_leakage_risks()` reports it as severed by subject or site
  grouping.

**Graph editing and export**

- `subset_graph(g, samples)`, `combine_graphs(g1, g2, ...)`, and
  `add_edges(g, edge_set)` derive new, validated graphs from existing ones
  without rebuilding from node/edge sets. Subsetting has the same semantics as
  `derive_split_constraints(samples = )`; combining regenerates edge ids and
  rejects conflicting definitions; adding edges preserves existing ids.
- `export_graph(g, file, format)` writes GraphML, GML, or node/edge CSV
  tables for Cytoscape, Gephi, networkx, and `igraph::read_graph()`, with the
  `attrs` list-column flattened to `attr_<name>` scalar columns.
- `plot(g, focus = "sample_projection")` draws the sample graph implied by the
  chosen `via` relations, and `plot(g, focus = "ego", node = )` draws one
  node's neighbourhood. The plot method is now documented (`?plot.dependency_graph`).

**Input and consumers**

- `graph_from_metadata()` is now an S3 generic with a `SummarizedExperiment`
  method (Bioconductor's \pkg{SummarizedExperiment} in Suggests): `colData()` is
  used as the metadata table and the assay column names become `sample_id`
  when no such column exists (`sample_id_col =` overrides).
- The adapter-cookbook vignette's `rsample` adapters are now executed when
  `rsample` is installed (it is in Suggests); the base-R adapter was already
  executed.
- `?as_split_spec` and the README gain a "What downstream consumers read"
  table, verified by running against bioLeak 0.3.8. The contract test now pins
  every row: bioLeak accepts exactly the subject / batch / study / time modes;
  it errors on site, region, platform, assay, relatedness and spatial (absent
  from its mode map) and also on composite (mapped to a `make_split_plan()`
  mode whose required arguments the adapter never supplies); and the
  documented workaround, joining `group_id` onto the observation frame and
  calling `make_split_plan()` directly, works. A bioLeak release that fixes
  either limitation will surface as a deliberate test failure to flip.
- The stale comment naming fastml as a consumer was removed; fastml has no
  splitGraph adapter.
- `inst/python/pyproject.toml` packages the Python reader as `splitspec`
  (stdlib-only, optional `pandas` / `sklearn` extras) for a PyPI release.

**Quality infrastructure and documentation**

- CI sets `NOT_CRAN=true`, installs Python, and so runs the Python
  conformance test in both the check and coverage workflows for the first
  time; a new Linux-only `perf-budget` workflow runs the performance tests.
- `dev/coverage.R` runs covr locally with a 90 % threshold; `dev/release.md`
  is the release checklist (schema bump rules, line-ending check for Windows
  builds, reverse-dependency check against bioLeak); `.lintr` and
  `_pkgdown.yml` (grouped reference index) are committed.
- New vignettes: **quick-start** (metadata frame to JSON `split_spec` with a
  mode decision table), **faq-design-notes** (why not `make_split_plan()`
  directly, composite over-merging, thresholds and transitive closure, the
  stratum annotation, schema versioning, conditions), and
  **case-study-gse60424** (a real public cohort: GEO series GSE60424, cached
  as `inst/extdata/GSE60424_samples.csv`).

**Conditions**

- **Classed conditions.** Every error raised by splitGraph now inherits from
  `splitgraph_error` — including every invalid enumerated argument (`mode`,
  `strategy`, `format`, `focus`, `layout`, `direction`, `outcome_scope`), which
  previously produced a bare `match.arg()` error; partial matching is
  unchanged. Each condition carries a `code` field, `NA` for plain argument
  checks; the documented
  subclasses are `splitgraph_schema_error`, `splitgraph_reference_error`,
  `splitgraph_ambiguity_error`, `splitgraph_validation_error`, and
  `splitgraph_io_error`. Package warnings carry `splitgraph_warning`. See
  `?splitgraph_conditions`. Messages are unchanged, so code matching on text
  keeps working.
- `relatedness_edges_from_kinship()` accepts a square kinship / GRM **matrix**
  with subject ids as row names (e.g. PLINK `--make-rel square` output) in
  addition to the long pair table.

## Bug fixes

- **Factor / numeric `site_id`, `region_id`, `platform_id` columns.**
  `graph_from_metadata()` failed with an `nzchar()` error when any of the three
  identifier columns introduced in 0.3.0 was a factor, because
  `ingest_metadata()` did not coerce them to character alongside the older
  identifier columns. All identifier columns are now coerced, and
  `create_nodes()` coerces its `id_col` defensively so factor identifiers work
  on the manual constructor path too.
- **`as_split_spec(constraint, graph = ...)` no longer aborts on ambiguous
  annotations.** Graph enrichment fills the blocking / ordering columns
  (`batch_group`, `study_group`, `site_group`, ..., `order_rank`) on a
  best-effort basis. A source that cannot be resolved unambiguously (for
  example a sample linked to two batches on a graph built with
  `validate = FALSE`) is now left as `NA` and reported in
  `metadata$enrichment_warnings` (also appended to `metadata$warnings`)
  instead of raising an error unrelated to the requested constraint mode.
- **Consistent `severed` column.** `summarize_leakage_risks()` and the
  `leakage_risk_summary()` constructor now include the `severed` column in the
  empty diagnostics table, matching the populated case.
- **Vector metadata fields are always JSON arrays.** `write_split_spec()` used
  `auto_unbox`, so a single-element `relations_used`, `warnings`, or
  `enrichment_warnings` was written as a bare string although the shipped schema
  declares `relations_used` an array. These fields are now written as arrays
  regardless of length; `read_split_spec()` already coerced them back to character
  vectors, so existing files still load.
- **Empty JSON objects are written as `{}`, not `[]`.** An empty
  `metadata$validation_overrides` (every graph built with default arguments),
  an empty `edge_sources`, and the metadata of a hand-built `split_spec()` were
  serialised as empty *arrays*, which the shipped schemas reject since they
  declare objects. The R-side validators now distinguish the two cases — an
  empty JSON object and an empty array both parse to a zero-length list, but
  only the object carries names — and check `attrs`, `metadata`,
  `validation_overrides` and `edge_sources`, so `read_*(validate = TRUE)` and
  `validate_graph_json()` catch the malformed shape. Nothing the package can
  write now fails its own validator, including edgeless graphs and specs with
  no metadata.
- **`add_edges()` accepts an edge set with no edges.** The thresholded helpers
  return an empty `graph_edge_set` when no pair passes, which previously
  crashed `add_edges()` with an unclassed error. The graph is now returned
  unchanged, with the threshold still recorded in `metadata$edge_sources`.
- **`build_dependency_graph()` rejects an unnamed `validation_overrides`
  list.** Overrides are looked up by name, so an unnamed list silently did
  nothing (and serialised as a JSON array).
- **Pairwise `threshold` / `threshold_metric` round-trip.** They are written as
  `null` for non-pairwise specs and now read back as typed `NA` rather than
  `NULL`, so a spec's metadata survives a write/read cycle unchanged.
- **An ambiguous sample-level outcome no longer borrows the subject's.** A
  sample linked to several `Outcome` nodes has no unique stratum and stays
  `NA`, instead of falling through to the `subject_has_outcome` label.

## Breaking changes

- **Removed the 0.1-era aliases deprecated in 0.2.0.** Migration:

  | Removed | Use instead |
  |---|---|
  | `new_depgraph_nodes()` | `graph_node_set()` |
  | `new_depgraph_edges()` | `graph_edge_set()` |
  | `new_depgraph()` | `dependency_graph()` |
  | `build_depgraph()` | `build_dependency_graph()` |
  | `validate_depgraph()` | `validate_graph()` |
  | `validate_graph(checks = ...)` | `validate_graph(levels = ..., severities = ...)` |

  Calling a removed alias is now a "could not find function" error; passing
  `checks=` is an "unused argument" error.
- **Composite and pairwise constraints no longer carry `metadata$projection_edges`.**
  The explicit sample-pair table was the quadratic part of the old derivation
  and is not needed for grouping. `metadata$n_dependency_edges` (the number of
  sample-target edges the components were computed from) replaces it; the pair
  table itself is still available from
  `detect_dependency_components()$metadata$projection_edges` and
  `detect_shared_dependencies()` when it is wanted.
- **Removed the unused `dependency_constraint()` constructor** and its
  `print` / `summary` / `as.data.frame` methods. Like the `leakage_constraint()`
  removed in 0.3.0, the class was exported but never produced or consumed
  anywhere in the package; `split_constraint()` (produced by
  `derive_split_constraints()`) is the supported constraint type.

## Documentation

- `?read_dependency_graph` no longer claims to return a *validated* graph: the
  reader checks table / `igraph` consistency but does not re-run
  `validate_graph()`, so a file written with `validate = FALSE` loads without
  error. Call `validate_graph()` on files from untrusted or older sources.
- `?graph_from_metadata` now lists `site_id`, `region_id`, and `platform_id`
  among the auto-detected columns and states that identifier columns may be
  character, factor, or numeric.
- `?derive_split_constraints` documents how composite `via` combines direct
  and pairwise sources (the restriction to direct sources noted early in this
  cycle was lifted; see "Pairwise sources inside composite derivations" above).

## Infrastructure

- The Python conformance test falls back to a `python` executable when
  `python3` is absent (the usual situation on Windows), provided it reports
  itself as Python 3.
- The stale `MD5` file was removed from the source tree. It is a build artefact
  that `R CMD build --md5` (the flag CRAN applies when it builds a submission)
  writes inside the tarball; a plain `R CMD build` does not create it, and it never
  belongs in the source tree.

# splitGraph 0.3.0

This release broadens the vocabulary of leakage relations splitGraph can model,
hardens the `split_spec` interchange format into a formally specified contract,
and demonstrates that contract across a language boundary. splitGraph continues
to stop at the constraint / `split_spec` boundary: generating folds, fitting
models, applying purge/embargo, and producing statistical leakage evidence
remain the responsibility of downstream consumers such as **bioLeak**.

## New features

**New leakage relations**

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
  (`sample_multiple_region_assignments`),
  `derive_split_constraints(mode = "region")`, a `region_group` blocking
  annotation in `split_spec`, and composite / plot support.
- **`Platform` node type and `sample_run_on_platform` edge, plus `platform` and
  `assay` constraint modes.** Sequencing / measurement-platform structure is now
  a first-class typed relation: `graph_from_metadata()` auto-detects a
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
  `derive_split_constraints(mode = "relatedness")` and `mode = "spatial"` form
  groups as connected components (transitive closure) over the thresholded
  edges, so a chain of individually near neighbours still lands in one group —
  a grouping that column-based approaches structurally cannot express. Both
  modes honor the `samples=` subset (components are recomputed within the subset,
  so an excluded bridge sample cannot leak structure across the split).

**Interchange-format hardening**

- **Formal JSON Schema.** The `dependency_graph` and `split_spec` on-disk formats
  now have formal JSON Schemas (Draft 2020-12) shipped in `inst/schema/`, and
  every written file references its schema via a `$schema` key.
- **JSON validators.** `validate_graph_json()` and `validate_split_spec_json()`
  check a handoff file against the contract — required fields, value types,
  node/edge-type enumerations, and referential integrity of edge endpoints —
  with no dependency beyond `jsonlite`.
- **Schema-version policy and one-shot upgraders.** `schema_version` is now
  `"0.2.0"`, and the compatibility rule is explicit and enforced: the major
  version is the compatibility boundary, so files sharing the installed major
  (including all `"0.1.0"` files) load silently, while a differing major loads
  with a warning. `migrate_dependency_graph_json()` and
  `migrate_split_spec_json()` upgrade an older file in place, filling fields
  introduced since it was written and stamping the current version and `$schema`.
- **Provenance.** `split_spec` metadata now records derivation provenance
  (`splitgraph_version`, `derived_at`) alongside the existing `source_mode` /
  `source_strategy` / `relations_used`.

**Cross-language interoperability**

- **Python reference consumer.** A pure-Python reader (`inst/python/splitspec/`)
  parses the `split_spec` JSON and exposes the grouping, ordering, and stratum
  annotations needed to drive scikit-learn `GroupKFold` /
  `StratifiedGroupKFold` / `TimeSeriesSplit` (the reader itself needs only the
  standard library). A conformance script (`inst/python/conformance.py`) and an
  accompanying R test assert that the Python reader recovers exactly the grouping
  (`grouping_vector()`) and `order_rank` that R emitted; the test is skipped when
  `python3` is absent and never runs on CRAN.

## Bug fixes

- **Composite-strict subset scoping.** `derive_split_constraints(mode =
  "composite", strategy = "strict", samples = ...)` now recomputes connected
  components *within* the requested subset. Previously two in-subset samples
  connected only through an out-of-subset sample could inherit a shared group,
  silently leaking excluded structure into the split.
- **Mode-aware leakage summaries.** `summarize_leakage_risks()` now reports a
  `severed` column indicating whether the chosen constraint mode structurally
  eliminates each leakage path (`TRUE`/`FALSE`), or `NA` when no constraint is
  supplied. Previously the same risk report was returned regardless of the mode.
- **`query_paths()` truncation is now visible.** When the search hits the finite
  `max_length` cap, the result metadata carries a `truncated = TRUE` flag so
  suppressed longer paths are no longer silent.
- **Timestamp round-trips.** A missing or unparseable `created_at` now round-trips
  as `NA` (POSIXct) instead of falling back to the current time.
- **`split_spec` JSON round-trip fidelity.** The new `site_group` / `region_group`
  / `platform_group` / `assay_group` columns are written and read back correctly;
  earlier development builds dropped them on serialization.

## Breaking changes

- **Removed the unused `leakage_constraint()` constructor** and its
  `print` / `summary` / `as.data.frame` methods. The object was exported but
  never produced or consumed anywhere in the package; `leakage_risk_summary()`
  (produced by `summarize_leakage_risks()`) is the supported leakage-reporting
  type and is unaffected.
- The data-model `schema_version` moved from `"0.1.0"` to `"0.2.0"`. This is
  backward compatible — existing `"0.1.0"` files load without warning — but the
  stamped version in newly written files changes.

## Documentation and infrastructure

- New vignette **`cross-language-handoff`**: the full R -> JSON -> Python ->
  scikit-learn path, showing `split_spec` as an interchange format rather than
  downstream plumbing.
- New vignette **`modeling-structure`**: modeling site, platform, assay,
  relatedness, and spatial structure end to end.
- The README gains a **"Scope & Relationship to bioLeak"** section, and the
  `?splitGraph` package doc now states scope and non-goals explicitly.
- A **contract test** (`Suggests: bioLeak`, skipped if absent) pins the seam to
  the reference consumer, asserting that a splitGraph `split_spec` satisfies
  `bioLeak::as_leaksplits()`.
- The package ships GitHub Actions workflows (multi-OS `R-CMD-check`,
  `test-coverage`) and a JOSS `paper.md` framed on the representation and
  interchange-format contribution.
- `stats` and `utils` are now declared in `Imports` (both were already used).

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

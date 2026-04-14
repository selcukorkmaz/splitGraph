# splitGraph: Dataset Dependency Graphs for Leakage-Aware Evaluation

`splitGraph` is an R package for representing biomedical dataset structure as a
typed dependency graph so that leakage-relevant relationships can be made
explicit, validated, queried, and converted into deterministic split
constraints.

It does not fit models, run preprocessing pipelines, or generate resamples by
itself. Its job is to encode dataset structure before evaluation so that
overlap, provenance, and time-ordering assumptions are inspectable instead of
implicit.

## Current Capabilities

What is implemented now:

- metadata ingestion with canonical ID normalization
- canonical node and edge constructors
- typed dependency-graph assembly backed by `igraph`
- structural, semantic, and leakage-relevant validation
- typed query and traversal helpers
- projected sample-dependency detection
- split-constraint derivation for subject, batch, study, time, and composite
  modes
- translation of constraints into stable sample-level split specifications
- split-spec preflight validation and leakage summary helpers
- S3 print, summary, plot, and `as.data.frame()` methods

## Why It Exists

In biomedical evaluation workflows, leakage often comes from dataset structure
rather than obvious coding mistakes. Samples may share:

- the same subject
- the same batch
- the same study
- the same collection timepoint
- the same assay provenance
- the same derived feature set
- the same outcome definition

If those relationships are not modeled explicitly, a train/test split can look
correct while still violating the intended scientific separation.

`splitGraph` makes those dependencies first-class objects.

## What The Package Does

`splitGraph` supports a full workflow:

1. Standardize metadata with `ingest_metadata()`.
2. Build typed nodes with `create_nodes()`.
3. Build typed edges with `create_edges()`.
4. Assemble a `dependency_graph` with `build_dependency_graph()`.
5. Validate the graph with `validate_graph()`.
6. Inspect or traverse the graph with the query helpers.
7. Derive split constraints with `derive_split_constraints()`.
8. Translate those constraints into a stable split specification with
   `as_split_spec()`.
9. Preflight the handoff with `validate_split_spec()` or summarize the
   overall state with `summarize_leakage_risks()`.

## What It Does Not Do

`splitGraph` is not:

- a gene network package
- a pathway graph package
- a general-purpose graph analytics toolkit
- a resampling engine
- a model training framework

The package is intentionally narrow: dataset dependency structure for
leakage-aware evaluation design.

## Design Guarantees

The package is intentionally strict in a few important places:

- requested sample subsets must resolve completely; unknown sample IDs error
  instead of being dropped
- ambiguous direct assignments for batch, study, and timepoint are rejected
- exact duplicate node and edge definitions may be collapsed, but conflicting
  duplicates are rejected rather than resolved by row order
- validation truth is not changed by severity filters
- contradictory time-order metadata are rejected instead of being resolved
  arbitrarily
- generated split specifications are validated against the package’s own
  preflight rules

## Installation

Development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("selcukorkmaz/splitGraph")
```

## Core Concepts

### Node types

The current package supports these canonical node types:

- `Sample`
- `Subject`
- `Batch`
- `Study`
- `Timepoint`
- `Assay`
- `FeatureSet`
- `Outcome`

### Canonical edge types

The current schema supports these canonical relations:

- `sample_belongs_to_subject`
- `sample_processed_in_batch`
- `sample_from_study`
- `sample_collected_at_timepoint`
- `sample_measured_by_assay`
- `sample_uses_featureset`
- `sample_has_outcome`
- `subject_has_outcome`
- `timepoint_precedes`
- `featureset_generated_from_study`
- `featureset_generated_from_batch`

### Main S3 objects

- `graph_node_set`
- `graph_edge_set`
- `dependency_graph`
- `depgraph_validation_report`
- `graph_query_result`
- `split_constraint`
- `split_spec`
- `split_spec_validation`
- `leakage_risk_summary`

## Main Functions

### Ingestion and construction

- `ingest_metadata()`
- `create_nodes()`
- `create_edges()`
- `build_dependency_graph()`
- `build_depgraph()`
- `dependency_graph()`
- `as_igraph()`

### Validation

- `validate_graph()`
- `validate_depgraph()`
- `validate_split_spec()`

### Queries

- `query_node_type()`
- `query_edge_type()`
- `query_neighbors()`
- `query_paths()`
- `query_shortest_paths()`
- `detect_dependency_components()`
- `detect_shared_dependencies()`

### Constraint derivation

- `derive_split_constraints()`
- `grouping_vector()`

### Split-spec translation

- `as_split_spec()`
- `summarize_leakage_risks()`

## Quick Start

```r
library(splitGraph)

meta <- ingest_metadata(
  data.frame(
    sample_id = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P1", "P2", "P3"),
    batch_id = c("B1", "B2", "B1", "B3"),
    study_id = c("ST1", "ST1", "ST2", "ST2"),
    timepoint_id = c("T0", "T1", "T0", "T2"),
    stringsAsFactors = FALSE
  ),
  dataset_name = "demo_dataset"
)

sample_nodes <- create_nodes(meta, type = "Sample", id_col = "sample_id")
subject_nodes <- create_nodes(meta, type = "Subject", id_col = "subject_id")
batch_nodes <- create_nodes(meta, type = "Batch", id_col = "batch_id")
study_nodes <- create_nodes(meta, type = "Study", id_col = "study_id")
time_nodes <- create_nodes(
  transform(
    unique(meta[!is.na(meta$timepoint_id), "timepoint_id", drop = FALSE]),
    time_index = c(0L, 1L, 2L)
  ),
  type = "Timepoint",
  id_col = "timepoint_id",
  attr_cols = "time_index"
)

subject_edges <- create_edges(
  meta, "sample_id", "subject_id",
  "Sample", "Subject", "sample_belongs_to_subject"
)
batch_edges <- create_edges(
  meta, "sample_id", "batch_id",
  "Sample", "Batch", "sample_processed_in_batch"
)
study_edges <- create_edges(
  meta, "sample_id", "study_id",
  "Sample", "Study", "sample_from_study"
)
time_edges <- create_edges(
  meta, "sample_id", "timepoint_id",
  "Sample", "Timepoint", "sample_collected_at_timepoint"
)

g <- build_dependency_graph(
  nodes = list(sample_nodes, subject_nodes, batch_nodes, study_nodes, time_nodes),
  edges = list(subject_edges, batch_edges, study_edges, time_edges),
  graph_name = "demo_graph",
  dataset_name = "demo_dataset"
)

validation <- validate_graph(g)
subject_constraint <- derive_split_constraints(g, mode = "subject")
time_constraint <- derive_split_constraints(g, mode = "time")
split_spec <- as_split_spec(time_constraint, graph = g)
split_spec_validation <- validate_split_spec(split_spec)
risk_summary <- summarize_leakage_risks(g, constraint = time_constraint, split_spec = split_spec)
```

## Typical Workflow

### 1. Ingest metadata

`ingest_metadata()` standardizes identifier-like columns, supports column
renaming through `col_map`, and attaches `dataset_name` as metadata.

Use it when raw metadata do not already match the expected column names.

### 2. Create canonical nodes and edges

`create_nodes()` converts a metadata frame into a `graph_node_set`.

Key behaviors:

- typed node IDs such as `sample:S1`
- optional labels
- optional attribute columns stored in `attrs`
- exact duplicate collapsing only when the retained definition is identical

`create_edges()` converts source and target columns into a `graph_edge_set`.

Key behaviors:

- validates known relation signatures
- optional endpoint dropping through `allow_missing = TRUE`
- exact duplicate collapsing only when attrs are identical
- support for prefixed and unprefixed endpoint IDs through `from_prefix` and
  `to_prefix`

### 3. Assemble and validate the graph

`build_dependency_graph()` merges canonical node and edge tables, builds the
internal `igraph`, and optionally validates the result before returning it.

`validate_graph()` returns a `depgraph_validation_report` with:

- `valid`
- `issues`
- `errors`
- `warnings`
- `advisories`
- `metrics`

Severity filters only trim the returned `issues` table. They do not change the
underlying `valid` result, and `error_on_fail = TRUE` still stops on hidden
errors.

Validation currently covers:

- identifier completeness and uniqueness
- edge endpoint integrity
- known edge signature enforcement
- single-target structural violations
- schema-level node-attribute checks
- conditional study-assignment warnings
- invalid feature-set and outcome metadata
- cyclic or contradictory time ordering
- graph-local leakage diagnostics such as repeated-subject structure and shared
  feature provenance

### 4. Query the dependency structure

The query layer returns `graph_query_result` objects with a tidy `table`
component and matching node/edge subsets.

Use:

- `query_node_type()` to inspect typed node subsets
- `query_edge_type()` to inspect typed edge subsets
- `query_neighbors()` for local neighborhoods
- `query_paths()` and `query_shortest_paths()` for directed traversal
- `detect_shared_dependencies()` to find sample pairs sharing a direct source
- `detect_dependency_components()` to project selected dependencies onto a
  sample graph and identify connected components

### 5. Derive split constraints

`derive_split_constraints()` converts the graph into deterministic
sample-level grouping rules.

Supported modes:

- `subject`
- `batch`
- `study`
- `time`
- `composite`

Composite strategies:

- `strict`: connected components in the projected sample dependency graph
- `rule_based`: deterministic priority fallback over available dependency
  sources

The returned `split_constraint$sample_map` always includes:

- `sample_id`
- `sample_node_id`
- `group_id`
- `constraint_type`
- `group_label`
- `explanation`

Time-aware outputs also include `timepoint_id`, `time_index`, and `order_rank`
when available.

### 6. Translate into a split specification

`as_split_spec()` converts a `split_constraint` into a stable
sample-level handoff object for downstream evaluation workflows.

The produced `split_spec` is tool-agnostic: downstream packages (bioLeak,
fastml, rsample, ...) consume it through their own adapters, so splitGraph
has no runtime dependency on any of them.

The translated `sample_data` always includes:

- `sample_id`
- `sample_node_id`
- `group_id`
- `primary_group`
- `batch_group`
- `study_group`
- `timepoint_id`
- `time_index`
- `order_rank`

Important detail:

- `time_var` may be present when ordering information exists for only some
  samples
- `ordering_required` is only set to `TRUE` when the constraint implies
  complete ordering coverage

`validate_split_spec()` performs preflight checks on the translated
object before downstream resampling is attempted.

## Example Queries

```r
query_node_type(g, "Subject")
query_edge_type(g, "sample_from_study")
query_neighbors(g, node_ids = "sample:S1", edge_types = "sample_belongs_to_subject")
query_paths(
  g,
  from = "sample:S1",
  to = "study:ST1",
  edge_types = "sample_from_study"
)
detect_shared_dependencies(g, via = "Batch")
detect_dependency_components(g, via = c("Subject", "Batch"))
```

## Example Split Designs

```r
subject_constraint <- derive_split_constraints(g, mode = "subject")
batch_constraint <- derive_split_constraints(g, mode = "batch")
study_constraint <- derive_split_constraints(g, mode = "study")
time_constraint <- derive_split_constraints(g, mode = "time")

strict_composite <- derive_split_constraints(
  g,
  mode = "composite",
  strategy = "strict",
  via = c("Subject", "Batch")
)

rule_based_composite <- derive_split_constraints(
  g,
  mode = "composite",
  strategy = "rule_based",
  priority = c("batch", "study", "subject", "time")
)
```

## Inspection Helpers

All core objects implement:

- `print()`
- `summary()`
- `as.data.frame()`

`dependency_graph` also supports:

- `plot()`

Useful helpers:

- `grouping_vector()` extracts a named `sample_id -> group_id` mapping from a
  `split_constraint`
- `as_igraph()` returns the internal traversal graph

## Downstream Integration

`splitGraph` prepares structure-aware evaluation inputs.

One supported handoff format is the package's `split_spec` object,
which provides stable sample-level grouping, blocking, and ordering fields.

That separation is intentional:

- `splitGraph` models and validates dataset structure
- downstream tooling decides how to operationalize the split specification

## Development Notes

This package currently emphasizes correctness and explicit failure over
convenience. If the graph is ambiguous, internally inconsistent, or
insufficiently specified for a requested operation, the package generally
errors instead of silently guessing.

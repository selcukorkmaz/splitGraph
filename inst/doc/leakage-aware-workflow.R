## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = TRUE
)

package_root <- if (file.exists("../DESCRIPTION")) ".." else "."
if (requireNamespace("pkgload", quietly = TRUE) &&
    file.exists(file.path(package_root, "DESCRIPTION"))) {
  pkgload::load_all(package_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(splitGraph)
}

or_empty <- function(x) {
  if (is.null(x)) character() else x
}

## ----metadata-----------------------------------------------------------------
meta <- data.frame(
  sample_id = c("S1", "S2", "S3", "S4", "S5", "S6"),
  subject_id = c("P1", "P1", "P2", "P3", "P4", "P2"),
  batch_id = c("B1", "B2", "B1", "B3", NA, "B1"),
  study_id = c("ST1", "ST1", "ST1", "ST2", "ST3", "ST2"),
  timepoint_id = c("T0", "T1", "T0", "T2", NA, "T1"),
  assay_id = c("RNAseq", "RNAseq", "RNAseq", "RNAseq", "Proteomics", "RNAseq"),
  featureset_id = c("FS_GLOBAL", "FS_GLOBAL", "FS_GLOBAL", "FS_GLOBAL", "FS_PROT", "FS_GLOBAL"),
  outcome_id = c("O_case", "O_case", "O_ctrl", "O_case", "O_ctrl", "O_ctrl"),
  stringsAsFactors = FALSE
)

meta

## ----fast-path----------------------------------------------------------------
quick_graph <- graph_from_metadata(
  data.frame(
    sample_id    = c("S1", "S2", "S3", "S4", "S5", "S6"),
    subject_id   = c("P1", "P1", "P2", "P2", "P3", "P3"),
    batch_id     = c("B1", "B2", "B1", "B2", "B1", "B2"),
    timepoint_id = c("T0", "T1", "T0", "T1", "T0", "T1"),
    time_index   = c(0, 1, 0, 1, 0, 1),
    outcome_value = c(0, 1, 0, 1, 1, 0)
  ),
  graph_name = "quick_demo"
)

quick_graph

## ----construction-------------------------------------------------------------
meta <- ingest_metadata(meta, dataset_name = "VignetteDemo")

sample_nodes <- create_nodes(meta, type = "Sample", id_col = "sample_id")
subject_nodes <- create_nodes(meta, type = "Subject", id_col = "subject_id")
batch_nodes <- create_nodes(meta, type = "Batch", id_col = "batch_id")
study_nodes <- create_nodes(meta, type = "Study", id_col = "study_id")

time_nodes <- create_nodes(
  data.frame(
    timepoint_id = c("T0", "T1", "T2"),
    time_index = c(0L, 1L, 2L),
    visit_label = c("baseline", "follow_up", "late_follow_up"),
    stringsAsFactors = FALSE
  ),
  type = "Timepoint",
  id_col = "timepoint_id",
  attr_cols = c("time_index", "visit_label")
)

assay_nodes <- create_nodes(
  data.frame(
    assay_id = c("RNAseq", "Proteomics"),
    modality = c("transcriptomics", "proteomics"),
    platform = c("NovaSeq", "Orbitrap"),
    stringsAsFactors = FALSE
  ),
  type = "Assay",
  id_col = "assay_id",
  attr_cols = c("modality", "platform")
)

featureset_nodes <- create_nodes(
  data.frame(
    featureset_id = c("FS_GLOBAL", "FS_PROT"),
    featureset_name = c("global_rna_signature", "proteomics_panel"),
    derivation_scope = c("per_dataset", "external"),
    feature_count = c(500L, 80L),
    stringsAsFactors = FALSE
  ),
  type = "FeatureSet",
  id_col = "featureset_id",
  attr_cols = c("featureset_name", "derivation_scope", "feature_count")
)

outcome_nodes <- create_nodes(
  data.frame(
    outcome_id = c("O_case", "O_ctrl"),
    outcome_name = c("response", "response"),
    outcome_type = c("binary", "binary"),
    observation_level = c("subject", "subject"),
    stringsAsFactors = FALSE
  ),
  type = "Outcome",
  id_col = "outcome_id",
  attr_cols = c("outcome_name", "outcome_type", "observation_level")
)

subject_edges <- create_edges(
  meta, "sample_id", "subject_id",
  "Sample", "Subject", "sample_belongs_to_subject"
)

batch_edges <- create_edges(
  meta, "sample_id", "batch_id",
  "Sample", "Batch", "sample_processed_in_batch",
  allow_missing = TRUE
)

study_edges <- create_edges(
  meta, "sample_id", "study_id",
  "Sample", "Study", "sample_from_study"
)

time_edges <- create_edges(
  meta, "sample_id", "timepoint_id",
  "Sample", "Timepoint", "sample_collected_at_timepoint",
  allow_missing = TRUE
)

assay_edges <- create_edges(
  meta, "sample_id", "assay_id",
  "Sample", "Assay", "sample_measured_by_assay"
)

featureset_edges <- create_edges(
  meta, "sample_id", "featureset_id",
  "Sample", "FeatureSet", "sample_uses_featureset"
)

outcome_edges <- create_edges(
  data.frame(
    subject_id = c("P1", "P2", "P3", "P4"),
    outcome_id = c("O_case", "O_ctrl", "O_case", "O_ctrl"),
    stringsAsFactors = FALSE
  ),
  "subject_id", "outcome_id",
  "Subject", "Outcome", "subject_has_outcome"
)

precedence_edges <- create_edges(
  data.frame(
    from_timepoint = c("T0", "T1"),
    to_timepoint = c("T1", "T2"),
    stringsAsFactors = FALSE
  ),
  "from_timepoint", "to_timepoint",
  "Timepoint", "Timepoint", "timepoint_precedes"
)

featureset_from_study <- create_edges(
  data.frame(
    featureset_id = "FS_GLOBAL",
    study_id = "ST1",
    stringsAsFactors = FALSE
  ),
  "featureset_id", "study_id",
  "FeatureSet", "Study", "featureset_generated_from_study"
)

featureset_from_batch <- create_edges(
  data.frame(
    featureset_id = "FS_GLOBAL",
    batch_id = "B1",
    stringsAsFactors = FALSE
  ),
  "featureset_id", "batch_id",
  "FeatureSet", "Batch", "featureset_generated_from_batch"
)

## ----construction-output------------------------------------------------------
sample_nodes
as.data.frame(sample_nodes)[, c("node_id", "node_type", "node_key", "label")]

edge_preview <- do.call(rbind, lapply(
  list(
    subject_edges, batch_edges, study_edges, time_edges,
    assay_edges, featureset_edges, outcome_edges,
    precedence_edges, featureset_from_study, featureset_from_batch
  ),
  as.data.frame
))

edge_preview[, c("from", "to", "edge_type")]

## ----graph--------------------------------------------------------------------
graph <- build_dependency_graph(
  nodes = list(
    sample_nodes, subject_nodes, batch_nodes, study_nodes,
    time_nodes, assay_nodes, featureset_nodes, outcome_nodes
  ),
  edges = list(
    subject_edges, batch_edges, study_edges, time_edges,
    assay_edges, featureset_edges, outcome_edges,
    precedence_edges, featureset_from_study, featureset_from_batch
  ),
  graph_name = "vignette_graph",
  dataset_name = attr(meta, "dataset_name")
)

graph
summary(graph)

## ----plot, fig.width = 7, fig.height = 5--------------------------------------
plot(graph)

## ----plot-options, eval = FALSE-----------------------------------------------
# plot(graph, layout = "sugiyama")         # alternative hierarchical layout
# plot(graph, show_labels = FALSE)         # hide labels on dense graphs
# plot(graph, legend = FALSE)              # suppress the legend
# plot(graph, legend_position = "bottomright")
# plot(graph, node_colors = c(Sample = "#000000"))

## ----validation---------------------------------------------------------------
validation <- validate_graph(graph)

validation
as.data.frame(validation)[, c("level", "severity", "code", "message")]

## ----strictness---------------------------------------------------------------
tryCatch(
  derive_split_constraints(graph, mode = "subject", samples = c("S1", "BAD")),
  error = function(e) e$message
)

## ----neighbors-and-paths------------------------------------------------------
neighbors_s1 <- query_neighbors(graph, node_ids = "sample:S1", direction = "out")
neighbors_s1
as.data.frame(neighbors_s1)[, c("seed_node_id", "node_id", "node_type", "edge_type")]

subject_outcome_path <- query_shortest_paths(
  graph,
  from = "sample:S1",
  to = "outcome:O_case",
  edge_types = c("sample_belongs_to_subject", "subject_has_outcome")
)

subject_outcome_path
as.data.frame(subject_outcome_path)

## ----projected-dependencies---------------------------------------------------
shared_dependencies <- detect_shared_dependencies(
  graph,
  via = c("Subject", "Batch", "FeatureSet")
)

as.data.frame(shared_dependencies)[, c(
  "sample_id_1", "sample_id_2", "shared_node_type", "shared_node_id", "edge_type"
)]

dependency_components <- detect_dependency_components(
  graph,
  via = c("Subject", "Batch")
)

as.data.frame(dependency_components)

## ----constraints--------------------------------------------------------------
subject_constraint <- derive_split_constraints(graph, mode = "subject")
batch_constraint <- derive_split_constraints(graph, mode = "batch")
study_constraint <- derive_split_constraints(graph, mode = "study")
time_constraint <- derive_split_constraints(graph, mode = "time")

strict_constraint <- derive_split_constraints(
  graph,
  mode = "composite",
  strategy = "strict",
  via = c("Subject", "Batch")
)

rule_based_constraint <- derive_split_constraints(
  graph,
  mode = "composite",
  strategy = "rule_based",
  priority = c("batch", "study", "subject", "time")
)

constraint_overview <- do.call(rbind, lapply(
  list(
    subject = subject_constraint,
    batch = batch_constraint,
    study = study_constraint,
    time = time_constraint,
    composite_strict = strict_constraint,
    composite_rule = rule_based_constraint
  ),
  function(x) {
    data.frame(
      strategy = x$strategy,
      groups = length(unique(x$sample_map$group_id)),
      warnings = if (is.null(x$metadata$warnings)) 0L else length(x$metadata$warnings),
      stringsAsFactors = FALSE
    )
  }
))

constraint_overview <- cbind(constraint = row.names(constraint_overview), constraint_overview)
row.names(constraint_overview) <- NULL

constraint_overview

## ----batch-constraint---------------------------------------------------------
batch_constraint
as.data.frame(batch_constraint)[, c("sample_id", "group_id", "group_label", "explanation")]

## ----time-constraint----------------------------------------------------------
time_constraint
as.data.frame(time_constraint)[, c("sample_id", "group_id", "timepoint_id", "order_rank")]

## ----composite-constraints----------------------------------------------------
strict_constraint
as.data.frame(strict_constraint)[, c("sample_id", "group_id", "constraint_type")]

rule_based_constraint
as.data.frame(rule_based_constraint)[, c("sample_id", "group_id", "constraint_type", "group_label")]

## ----precedence-only----------------------------------------------------------
precedence_meta <- data.frame(
  sample_id = c("S1", "S2", "S3"),
  subject_id = c("P1", "P1", "P2"),
  study_id = c("ST1", "ST1", "ST2"),
  timepoint_id = c("T0", "T1", "T2"),
  stringsAsFactors = FALSE
)

precedence_graph <- build_dependency_graph(
  nodes = list(
    create_nodes(precedence_meta, type = "Sample", id_col = "sample_id"),
    create_nodes(precedence_meta, type = "Subject", id_col = "subject_id"),
    create_nodes(precedence_meta, type = "Study", id_col = "study_id"),
    create_nodes(
      data.frame(timepoint_id = c("T0", "T1", "T2"), stringsAsFactors = FALSE),
      type = "Timepoint",
      id_col = "timepoint_id"
    )
  ),
  edges = list(
    create_edges(
      precedence_meta, "sample_id", "subject_id",
      "Sample", "Subject", "sample_belongs_to_subject"
    ),
    create_edges(
      precedence_meta, "sample_id", "study_id",
      "Sample", "Study", "sample_from_study"
    ),
    create_edges(
      precedence_meta, "sample_id", "timepoint_id",
      "Sample", "Timepoint", "sample_collected_at_timepoint"
    ),
    create_edges(
      data.frame(
        from_timepoint = c("T0", "T1"),
        to_timepoint = c("T1", "T2"),
        stringsAsFactors = FALSE
      ),
      "from_timepoint", "to_timepoint",
      "Timepoint", "Timepoint", "timepoint_precedes"
    )
  ),
  graph_name = "precedence_only_graph"
)

precedence_time_constraint <- derive_split_constraints(precedence_graph, mode = "time")

precedence_time_constraint$metadata$time_order_source
as.data.frame(precedence_time_constraint)[, c("sample_id", "timepoint_id", "time_index", "order_rank")]

## ----split-spec---------------------------------------------------------------
split_spec <- as_split_spec(strict_constraint, graph = graph)
split_spec

as.data.frame(split_spec)[, c(
  "sample_id", "group_id", "batch_group", "study_group", "timepoint_id", "order_rank"
)]

split_spec_validation <- validate_split_spec(split_spec)
split_spec_validation
as.data.frame(split_spec_validation)

## ----risk-summary-------------------------------------------------------------
risk_summary <- summarize_leakage_risks(
  graph,
  constraint = strict_constraint,
  split_spec = split_spec
)

risk_summary
as.data.frame(risk_summary)[, c("source", "severity", "category", "message")]

## ----case-study-1-------------------------------------------------------------
subject_groups <- grouping_vector(subject_constraint)
time_groups <- time_constraint$sample_map[, c("sample_id", "group_id", "timepoint_id", "order_rank")]

subject_groups
time_groups

## ----case-study-2-------------------------------------------------------------
cross_study_issues <- as.data.frame(validation)[
  as.data.frame(validation)$code == "subject_cross_study_overlap",
  c("severity", "code", "message")
]

p2_shared <- detect_shared_dependencies(
  graph,
  via = "Subject",
  samples = c("S3", "S6")
)

study_only_map <- study_constraint$sample_map[, c("sample_id", "group_id", "group_label")]
strict_map <- strict_constraint$sample_map[, c("sample_id", "group_id", "constraint_type")]

cross_study_issues
as.data.frame(p2_shared)
study_only_map[study_only_map$sample_id %in% c("S3", "S6"), ]
strict_map[strict_map$sample_id %in% c("S3", "S6"), ]

## ----case-study-3-------------------------------------------------------------
batch_missing <- batch_constraint$sample_map[
  batch_constraint$sample_map$sample_id == "S5",
  c("sample_id", "group_id", "group_label", "explanation")
]

rule_based_missing <- rule_based_constraint$sample_map[
  rule_based_constraint$sample_map$sample_id == "S5",
  c("sample_id", "group_id", "constraint_type", "group_label", "explanation")
]

split_spec_missing <- as.data.frame(split_spec)[
  as.data.frame(split_spec)$sample_id == "S5",
  c("sample_id", "group_id", "batch_group", "study_group", "timepoint_id", "order_rank")
]

batch_missing
rule_based_missing
split_spec_missing

## ----case-study-4-------------------------------------------------------------
strategy_summary <- data.frame(
  constraint = c("subject", "batch", "study", "time", "composite_strict", "composite_rule"),
  groups = c(
    length(unique(subject_constraint$sample_map$group_id)),
    length(unique(batch_constraint$sample_map$group_id)),
    length(unique(study_constraint$sample_map$group_id)),
    length(unique(time_constraint$sample_map$group_id)),
    length(unique(strict_constraint$sample_map$group_id)),
    length(unique(rule_based_constraint$sample_map$group_id))
  ),
  warnings = c(
    length(or_empty(subject_constraint$metadata$warnings)),
    length(or_empty(batch_constraint$metadata$warnings)),
    length(or_empty(study_constraint$metadata$warnings)),
    length(or_empty(time_constraint$metadata$warnings)),
    length(or_empty(strict_constraint$metadata$warnings)),
    length(or_empty(rule_based_constraint$metadata$warnings))
  ),
  recommended_resampling = c(
    as_split_spec(subject_constraint, graph = graph)$recommended_resampling,
    as_split_spec(batch_constraint, graph = graph)$recommended_resampling,
    as_split_spec(study_constraint, graph = graph)$recommended_resampling,
    as_split_spec(time_constraint, graph = graph)$recommended_resampling,
    as_split_spec(strict_constraint, graph = graph)$recommended_resampling,
    as_split_spec(rule_based_constraint, graph = graph)$recommended_resampling
  ),
  stringsAsFactors = FALSE
)

strategy_summary


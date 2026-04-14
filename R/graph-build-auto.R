# One-shot builder that derives nodes, edges, and timepoint ordering from
# canonical metadata columns.

.depgraph_auto_specs <- list(
  list(type = "Subject",    id_col = "subject_id",    relation = "sample_belongs_to_subject"),
  list(type = "Batch",      id_col = "batch_id",      relation = "sample_processed_in_batch"),
  list(type = "Study",      id_col = "study_id",      relation = "sample_from_study"),
  list(type = "Timepoint",  id_col = "timepoint_id",  relation = "sample_collected_at_timepoint"),
  list(type = "Assay",      id_col = "assay_id",      relation = "sample_measured_by_assay"),
  list(type = "FeatureSet", id_col = "featureset_id", relation = "sample_uses_featureset")
)

graph_from_metadata <- function(meta,
                                columns = NULL,
                                dataset_name = NULL,
                                graph_name = NULL,
                                outcome_scope = c("sample", "subject"),
                                time_precedence = TRUE,
                                validate = TRUE,
                                validation_overrides = list()) {
  .depgraph_assert(is.data.frame(meta), "`meta` must be a data.frame.")
  outcome_scope <- match.arg(outcome_scope)

  meta <- ingest_metadata(meta, col_map = columns, dataset_name = dataset_name, strict = TRUE)

  sample_nodes <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  node_sets <- list(sample_nodes)
  edge_sets <- list()

  for (spec in .depgraph_auto_specs) {
    if (!(spec$id_col %in% names(meta))) next
    present <- !is.na(meta[[spec$id_col]]) & nzchar(as.character(meta[[spec$id_col]]))
    if (!any(present)) next
    sub <- meta[present, , drop = FALSE]

    node_sets[[length(node_sets) + 1L]] <- create_nodes(
      sub, type = spec$type, id_col = spec$id_col
    )
    edge_sets[[length(edge_sets) + 1L]] <- create_edges(
      sub,
      from_col = "sample_id", to_col = spec$id_col,
      from_type = "Sample", to_type = spec$type,
      relation = spec$relation
    )
  }

  has_timepoint <- "timepoint_id" %in% names(meta) &&
    any(!is.na(meta$timepoint_id) & nzchar(as.character(meta$timepoint_id)))
  if (isTRUE(time_precedence) && has_timepoint && "time_index" %in% names(meta)) {
    tp_rows <- !is.na(meta$timepoint_id) & nzchar(as.character(meta$timepoint_id)) &
      !is.na(meta$time_index)
    if (any(tp_rows)) {
      tp_df <- unique(data.frame(
        timepoint_id = as.character(meta$timepoint_id[tp_rows]),
        time_index = meta$time_index[tp_rows],
        stringsAsFactors = FALSE
      ))
      tp_df <- tp_df[order(tp_df$time_index, tp_df$timepoint_id), , drop = FALSE]
      tp_df <- tp_df[!duplicated(tp_df$timepoint_id), , drop = FALSE]
      if (nrow(tp_df) >= 2L) {
        precedes_df <- data.frame(
          from_id = tp_df$timepoint_id[-nrow(tp_df)],
          to_id = tp_df$timepoint_id[-1L],
          stringsAsFactors = FALSE
        )
        edge_sets[[length(edge_sets) + 1L]] <- create_edges(
          precedes_df,
          from_col = "from_id", to_col = "to_id",
          from_type = "Timepoint", to_type = "Timepoint",
          relation = "timepoint_precedes"
        )
      }
    }
  }

  outcome_col <- if ("outcome_id" %in% names(meta)) "outcome_id" else if ("outcome_value" %in% names(meta)) "outcome_value" else NULL
  if (!is.null(outcome_col)) {
    present <- !is.na(meta[[outcome_col]]) & nzchar(as.character(meta[[outcome_col]]))
    if (any(present)) {
      sub <- meta[present, , drop = FALSE]
      sub$outcome_id <- as.character(sub[[outcome_col]])
      node_sets[[length(node_sets) + 1L]] <- create_nodes(
        sub, type = "Outcome", id_col = "outcome_id"
      )
      if (identical(outcome_scope, "subject") && "subject_id" %in% names(sub)) {
        edge_sets[[length(edge_sets) + 1L]] <- create_edges(
          sub,
          from_col = "subject_id", to_col = "outcome_id",
          from_type = "Subject", to_type = "Outcome",
          relation = "subject_has_outcome"
        )
      } else {
        edge_sets[[length(edge_sets) + 1L]] <- create_edges(
          sub,
          from_col = "sample_id", to_col = "outcome_id",
          from_type = "Sample", to_type = "Outcome",
          relation = "sample_has_outcome"
        )
      }
    }
  }

  build_dependency_graph(
    nodes = node_sets,
    edges = edge_sets,
    graph_name = graph_name,
    dataset_name = dataset_name,
    validate = validate,
    validation_overrides = validation_overrides
  )
}

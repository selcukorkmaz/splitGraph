# One-shot builder that derives nodes, edges, and timepoint ordering from
# canonical metadata columns.

.depgraph_auto_specs <- list(
  list(type = "Subject",    id_col = "subject_id",    relation = "sample_belongs_to_subject"),
  list(type = "Batch",      id_col = "batch_id",      relation = "sample_processed_in_batch"),
  list(type = "Study",      id_col = "study_id",      relation = "sample_from_study"),
  list(type = "Timepoint",  id_col = "timepoint_id",  relation = "sample_collected_at_timepoint"),
  list(type = "Assay",      id_col = "assay_id",      relation = "sample_measured_by_assay"),
  list(type = "FeatureSet", id_col = "featureset_id", relation = "sample_uses_featureset"),
  list(type = "Site",       id_col = "site_id",       relation = "sample_collected_at_site"),
  list(type = "Region",     id_col = "region_id",     relation = "sample_located_in_region"),
  list(type = "Platform",   id_col = "platform_id",   relation = "sample_run_on_platform")
)

#' Build a Dependency Graph Directly from a Metadata Table
#'
#' One-shot convenience builder that auto-detects canonical columns in a
#' metadata table, creates the corresponding node and edge sets, optionally
#' derives timepoint ordering from \code{time_index}, and assembles a
#' \code{dependency_graph}. Columns that are absent or entirely missing are
#' silently skipped.
#'
#' @param meta A \code{data.frame} containing one row per sample and optional
#'   canonical columns: \code{sample_id} (required), \code{subject_id},
#'   \code{batch_id}, \code{study_id}, \code{timepoint_id}, \code{time_index},
#'   \code{assay_id}, \code{featureset_id}, \code{site_id}, \code{region_id},
#'   \code{platform_id}, \code{outcome_id}, or \code{outcome_value}.
#'   Identifier columns may be character, factor, or numeric; they are
#'   coerced to character by \code{ingest_metadata()}.
#' @param columns Optional named character vector passed to
#'   \code{ingest_metadata()} to rename user columns to canonical names.
#' @param dataset_name,graph_name Optional metadata labels.
#' @param outcome_scope Either \code{"sample"} (default) or \code{"subject"}.
#'   Controls whether outcome edges attach to samples or subjects.
#' @param time_precedence If \code{TRUE} and \code{time_index} is present,
#'   derive \code{timepoint_precedes} edges from the ordering of
#'   \code{time_index}.
#' @param validate Forwarded to \code{build_dependency_graph()}.
#' @param validation_overrides Forwarded to \code{build_dependency_graph()}.
#' @param ... Passed on to the \code{data.frame} method.
#' @param sample_id_col For the \code{SummarizedExperiment} method: the
#'   \code{colData} column holding sample identifiers. When \code{NULL}
#'   (default) a \code{sample_id} column is used if present, otherwise the
#'   assay column names (\code{colnames(se)}) become the sample identifiers.
#'   For the \code{MultiAssayExperiment} method: the \code{sampleMap} column
#'   holding assay sample identifiers (default \code{"colname"}).
#' @param subject_id_col,assay_id_col For the \code{MultiAssayExperiment}
#'   method: \code{sampleMap} columns holding subject (\code{"primary"}) and
#'   assay (\code{"assay"}) identifiers.
#' @return A validated \code{dependency_graph}.
#' @details
#' \code{graph_from_metadata()} is an S3 generic. The \code{data.frame} method
#' is the one described above. The \code{SummarizedExperiment} method (used
#' when Bioconductor's \pkg{SummarizedExperiment} is installed) converts
#' \code{colData(se)} to a data frame, adds \code{sample_id} from the assay
#' column names when \code{colData} has no such column, and dispatches to the
#' \code{data.frame} method; \code{columns} maps \code{colData} names to the
#' canonical ones exactly as for a data frame. A worked example is in
#' \code{vignette("faq-design-notes")}; it is kept out of the examples below
#' because attaching Bioconductor packages dominates their run time.
#' @examples
#' meta <- data.frame(
#'   sample_id  = c("S1", "S2", "S3", "S4"),
#'   subject_id = c("P1", "P1", "P2", "P2"),
#'   batch_id   = c("B1", "B2", "B1", "B2"),
#'   timepoint_id = c("T1", "T2", "T1", "T2"),
#'   time_index = c(1, 2, 1, 2),
#'   outcome_id = c("ctrl", "case", "ctrl", "case")
#' )
#'
#' g <- graph_from_metadata(meta, graph_name = "demo")
#' g
#' @export
graph_from_metadata <- function(meta, ...) {
  UseMethod("graph_from_metadata")
}

#' @rdname graph_from_metadata
#' @export
graph_from_metadata.default <- function(meta, ...) {
  .depgraph_stop(
    paste0(
      "`meta` must be a data.frame or a SummarizedExperiment; got <",
      paste(class(meta), collapse = "/"), ">."
    ),
    class = "splitgraph_schema_error", code = "unsupported_metadata_input"
  )
}

#' @rdname graph_from_metadata
#' @export
graph_from_metadata.SummarizedExperiment <- function(meta, ..., sample_id_col = NULL) {
  .depgraph_assert(
    requireNamespace("SummarizedExperiment", quietly = TRUE),
    "Package 'SummarizedExperiment' is required to build a graph from a SummarizedExperiment."
  )
  col_data <- as.data.frame(SummarizedExperiment::colData(meta), stringsAsFactors = FALSE, optional = TRUE)
  sample_names <- colnames(meta)

  if (!is.null(sample_id_col)) {
    .depgraph_assert(
      sample_id_col %in% names(col_data),
      paste0("`sample_id_col` not found in colData: ", sample_id_col),
      class = "splitgraph_reference_error", code = "missing_sample_id_column"
    )
    col_data$sample_id <- as.character(col_data[[sample_id_col]])
  } else if (!"sample_id" %in% names(col_data)) {
    .depgraph_assert(
      !is.null(sample_names) && all(nzchar(sample_names)),
      "The SummarizedExperiment has no `sample_id` column in colData and no column names to use instead."
    )
    col_data$sample_id <- as.character(sample_names)
  }
  row.names(col_data) <- NULL
  graph_from_metadata.data.frame(col_data, ...)
}

#' @rdname graph_from_metadata
#' @export
graph_from_metadata.MultiAssayExperiment <- function(meta, ..., sample_id_col = "colname", subject_id_col = "primary", assay_id_col = "assay") {
  smap <- if (!isS4(meta) && is.list(meta) && !is.null(meta$sampleMap)) {
    as.data.frame(meta$sampleMap, stringsAsFactors = FALSE)
  } else if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    as.data.frame(MultiAssayExperiment::sampleMap(meta), stringsAsFactors = FALSE)
  } else {
    .depgraph_stop(
      "Package 'MultiAssayExperiment' is required to extract sampleMap from a MultiAssayExperiment.",
      class = "splitgraph_input_error", code = "missing_multiassayexperiment"
    )
  }

  for (col in c(sample_id_col, subject_id_col, assay_id_col)) {
    .depgraph_assert(
      col %in% names(smap),
      paste0("Column `", col, "` not found in MultiAssayExperiment sampleMap."),
      class = "splitgraph_reference_error", code = "missing_samplemap_column"
    )
  }

  df <- data.frame(
    sample_id = as.character(smap[[sample_id_col]]),
    subject_id = as.character(smap[[subject_id_col]]),
    assay_id = as.character(smap[[assay_id_col]]),
    stringsAsFactors = FALSE
  )

  cdata <- if (!isS4(meta) && is.list(meta) && !is.null(meta$colData)) {
    as.data.frame(meta$colData, stringsAsFactors = FALSE, optional = TRUE)
  } else if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    as.data.frame(MultiAssayExperiment::colData(meta), stringsAsFactors = FALSE, optional = TRUE)
  } else {
    NULL
  }

  if (!is.null(cdata) && nrow(cdata) > 0L) {
    subj_keys <- if ("subject_id" %in% names(cdata)) as.character(cdata$subject_id) else rownames(cdata)
    if (!is.null(subj_keys) && length(subj_keys) == nrow(cdata)) {
      idx <- match(df$subject_id, subj_keys)
      extra_cols <- setdiff(names(cdata), c("sample_id", "subject_id", "assay_id"))
      for (ec in extra_cols) {
        df[[ec]] <- cdata[[ec]][idx]
      }
    }
  }
  row.names(df) <- NULL
  graph_from_metadata.data.frame(df, ...)
}

.auto_entity_attr_cols <- function(df, type, id_col) {
  schema <- .depgraph_node_type_schema(type)
  candidates <- setdiff(intersect(schema$optional_attrs, names(df)), id_col)
  if (length(candidates) == 0L) {
    return(character())
  }
  ids <- as.character(df[[id_col]])
  keep <- vapply(candidates, function(col) {
    # Always retain `time_index` on Timepoint so conflicting time indices for
    # the same timepoint_id surface as a node-definition ambiguity error.
    if (identical(type, "Timepoint") && identical(col, "time_index")) {
      return(TRUE)
    }
    vals <- df[[col]]
    distinct_pairs <- unique(data.frame(id = ids, val = vals, stringsAsFactors = FALSE))
    anyDuplicated(distinct_pairs$id) == 0L
  }, logical(1))
  candidates[keep]
}

#' @rdname graph_from_metadata
#' @export
graph_from_metadata.data.frame <- function(meta,
                                           columns = NULL,
                                           dataset_name = NULL,
                                           graph_name = NULL,
                                           outcome_scope = c("sample", "subject"),
                                           time_precedence = TRUE,
                                           validate = TRUE,
                                           validation_overrides = list(),
                                           ...) {
  .depgraph_assert(is.data.frame(meta), "`meta` must be a data.frame.")
  outcome_scope <- .depgraph_match_arg(outcome_scope, c("sample", "subject"), "outcome_scope")

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
      sub,
      type = spec$type,
      id_col = spec$id_col,
      attr_cols = .auto_entity_attr_cols(sub, spec$type, spec$id_col)
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
    num_time_index <- suppressWarnings(as.numeric(as.character(meta$time_index)))
    tp_rows <- !is.na(meta$timepoint_id) & nzchar(as.character(meta$timepoint_id)) &
      !is.na(num_time_index)
    if (any(tp_rows)) {
      tp_df <- unique(data.frame(
        timepoint_id = as.character(meta$timepoint_id[tp_rows]),
        time_index = num_time_index[tp_rows],
        stringsAsFactors = FALSE
      ))
      tp_df <- tp_df[order(tp_df$time_index, tp_df$timepoint_id), , drop = FALSE]
      tp_df <- tp_df[!duplicated(tp_df$timepoint_id), , drop = FALSE]
      # Only auto-create strict `timepoint_precedes` edges when distinct
      # timepoints have strictly increasing `time_index` values; tied
      # `time_index` values across different timepoint_ids are valid in
      # `time_index` mode but would violate strict `<` in `timepoint_precedes`.
      if (nrow(tp_df) >= 2L && anyDuplicated(tp_df$time_index) == 0L) {
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
    if (identical(outcome_scope, "subject") && !("subject_id" %in% names(meta))) {
      .depgraph_stop(
        "`outcome_scope = \"subject\"` requires a `subject_id` column in `meta`.",
        class = "splitgraph_input_error",
        code = "missing_subject_for_outcome_scope"
      )
    }
    present <- !is.na(meta[[outcome_col]]) & nzchar(as.character(meta[[outcome_col]]))
    if (identical(outcome_scope, "subject")) {
      present <- present & !is.na(meta$subject_id) & nzchar(as.character(meta$subject_id))
    }
    if (any(present)) {
      sub <- meta[present, , drop = FALSE]
      if (identical(outcome_col, "outcome_value") && is.numeric(meta[[outcome_col]])) {
        .depgraph_warn(
          c("`outcome_value` is numeric: graph_from_metadata() will create one ",
            "Outcome node per distinct numeric value (e.g. `outcome:0`, `outcome:1`). ",
            "If you intended a class label, pass `outcome_id` (character) instead, ",
            "or coerce `outcome_value` to a character class label first."),
          code = "numeric_outcome_value"
        )
      }
      sub$outcome_id <- as.character(sub[[outcome_col]])
      node_sets[[length(node_sets) + 1L]] <- create_nodes(
        sub,
        type = "Outcome",
        id_col = "outcome_id",
        attr_cols = .auto_entity_attr_cols(sub, "Outcome", "outcome_id")
      )
      if (identical(outcome_scope, "subject")) {
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
    # `sample_id` is the only required column, so a table carrying nothing else
    # is legitimate and must yield an edgeless graph rather than an internal
    # "non-empty list" error from the binder.
    edges = if (length(edge_sets) == 0L) list(graph_edge_set()) else edge_sets,
    graph_name = graph_name,
    dataset_name = dataset_name,
    validate = validate,
    validation_overrides = validation_overrides
  )
}

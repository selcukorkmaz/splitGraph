# Graph editing: subset to a sample set, combine graphs, add edge sets.

# Give every edge an id of the form "<edge_type>:<k>", numbering within each
# edge type. Ids in `reserve` are left untouched and their indices skipped, so
# edges added to an existing graph never collide with ids callers already hold.
.depgraph_renumber_edge_ids <- function(edge_data, reserve = character()) {
  if (nrow(edge_data) == 0L) return(edge_data)
  keep <- !is.na(edge_data$edge_id) & edge_data$edge_id %in% reserve
  new_ids <- edge_data$edge_id
  for (type in unique(edge_data$edge_type[!keep])) {
    idx <- which(!keep & edge_data$edge_type == type)
    used <- suppressWarnings(as.integer(sub("^.*:", "", edge_data$edge_id[keep & edge_data$edge_type == type])))
    start <- if (length(used) == 0L || all(is.na(used))) 0L else max(used, na.rm = TRUE)
    new_ids[idx] <- paste0(type, ":", start + seq_along(idx))
  }
  edge_data$edge_id <- new_ids
  edge_data
}

# Drop exact duplicate (from, to, edge_type, attrs) rows; error on rows that
# share (from, to, edge_type) but differ in attrs.
.depgraph_dedupe_edges <- function(edge_data) {
  if (nrow(edge_data) == 0L) return(edge_data)
  key <- paste(edge_data$from, edge_data$to, edge_data$edge_type, sep = "\r")
  attr_key <- vapply(edge_data$attrs, function(a) paste(deparse(a), collapse = ""), character(1))
  full_key <- paste(key, attr_key, sep = "\r")
  distinct <- !duplicated(full_key)
  conflicting <- unique(key[distinct][duplicated(key[distinct])])
  if (length(conflicting) > 0L) {
    labels <- vapply(strsplit(conflicting, "\r", fixed = TRUE), function(p) paste0(p[[1L]], " -> ", p[[2L]], " [", p[[3L]], "]"), character(1))
    .depgraph_stop(
      paste0("Conflicting edge definitions found for relations: ", paste(labels, collapse = ", ")),
      class = "splitgraph_ambiguity_error", code = "conflicting_edge_definitions"
    )
  }
  out <- edge_data[distinct, , drop = FALSE]
  row.names(out) <- NULL
  out
}

# Drop exact duplicate node rows; error on rows that share node_id but differ.
.depgraph_dedupe_nodes <- function(node_data) {
  if (nrow(node_data) == 0L) return(node_data)
  attr_key <- vapply(node_data$attrs, function(a) paste(deparse(a), collapse = ""), character(1))
  full_key <- paste(node_data$node_id, node_data$node_type, node_data$node_key, node_data$label, attr_key, sep = "\r")
  distinct <- !duplicated(full_key)
  ids <- node_data$node_id[distinct]
  conflicting <- unique(ids[duplicated(ids)])
  if (length(conflicting) > 0L) {
    .depgraph_stop(
      paste0("Conflicting node definitions found for IDs: ", paste(conflicting, collapse = ", ")),
      class = "splitgraph_ambiguity_error", code = "conflicting_node_definitions"
    )
  }
  out <- node_data[distinct, , drop = FALSE]
  row.names(out) <- NULL
  out
}

# Rebuild a dependency_graph from edited tables, carrying over the metadata
# that build_dependency_graph() cannot reconstruct from node/edge sets alone.
.depgraph_rebuild <- function(node_data, edge_data, metadata, graph_name, dataset_name, validate, extra_metadata = list()) {
  out <- build_dependency_graph(
    nodes = list(graph_node_set(node_data)),
    edges = list(graph_edge_set(edge_data)),
    graph_name = graph_name,
    dataset_name = dataset_name,
    validate = validate,
    validation_overrides = metadata$validation_overrides %||% list()
  )
  sources <- metadata$edge_sources %||% list()
  out$metadata$edge_sources <- sources[names(sources) %in% unique(edge_data$edge_type)]
  out$metadata <- utils::modifyList(out$metadata, extra_metadata)
  out
}

#' Edit Dependency Graphs
#'
#' Derive a new \code{dependency_graph} from existing ones without rebuilding
#' from node and edge sets: restrict a graph to a subset of samples, take the
#' union of several graphs, or append edge sets to a graph. Every function
#' returns a new, independently validated \code{dependency_graph}; the inputs
#' are never modified.
#'
#' \code{subset_graph()} keeps the requested \code{Sample} nodes, every edge
#' rooted at one of them (a \code{sample_adjacent_to} edge is kept only when
#' both samples are kept), the non-sample nodes those edges point to, and,
#' transitively, non-sample nodes reachable from kept nodes through
#' non-sample edges (\code{assay_uses_platform},
#' \code{featureset_generated_from_*}, \code{subject_related_to},
#' \code{subject_has_outcome}). \code{timepoint_precedes} edges are kept only
#' between retained timepoints, so when \code{time_index} is absent the
#' ordering of a subset may become partial; \code{derive_split_constraints(mode
#' = "time")} reports that in its warnings. Restricting the graph this way has
#' the same semantics as the \code{samples} argument of
#' \code{\link{derive_split_constraints}}: structure that only reaches the
#' subset through excluded samples is dropped.
#'
#' \code{combine_graphs()} takes the union of node and edge tables. Identical
#' rows are collapsed; a node id or an \code{(from, to, edge_type)} relation
#' defined differently in two graphs is an error of class
#' \code{splitgraph_ambiguity_error}. Edge ids are regenerated per edge type
#' (\code{"<edge_type>:<k>"}), since ids from different graphs would collide.
#' Metadata \code{validation_overrides} and \code{edge_sources} are merged with
#' later graphs taking precedence.
#'
#' \code{add_edges()} appends one or more \code{graph_edge_set}s (for example
#' the output of \code{\link{relatedness_edges_from_kinship}}) to a graph. New
#' edges receive ids that continue the existing numbering of their edge type;
#' existing ids are preserved. Endpoints must already exist in the graph.
#'
#' @param graph A \code{dependency_graph}.
#' @param samples Sample identifiers or sample node ids to keep. All must
#'   resolve; unknown ids raise a \code{splitgraph_reference_error}.
#' @param ... For \code{combine_graphs()}, two or more \code{dependency_graph}s
#'   (or a single list of them).
#' @param edges A \code{graph_edge_set} or a list of them.
#' @param graph_name,dataset_name Optional labels for the result. When
#'   \code{NULL}, \code{subset_graph()} and \code{add_edges()} inherit the
#'   input's labels, and \code{combine_graphs()} uses the first non-\code{NULL}
#'   label among its inputs.
#' @param validate If \code{TRUE} (default), run \code{validate_graph()} on the
#'   result and fail on error-severity issues, as
#'   \code{build_dependency_graph()} does.
#' @return A \code{dependency_graph}.
#' @examples
#' meta <- data.frame(
#'   sample_id  = c("S1", "S2", "S3", "S4"),
#'   subject_id = c("P1", "P1", "P2", "P3"),
#'   batch_id   = c("B1", "B1", "B2", "B2")
#' )
#' g <- graph_from_metadata(meta, graph_name = "full")
#'
#' g_sub <- subset_graph(g, samples = c("S1", "S2"))
#' summary(g_sub)$node_types
#'
#' pairs <- data.frame(id1 = "P1", id2 = "P2", kinship = 0.25)
#' g_kin <- add_edges(g, relatedness_edges_from_kinship(pairs, threshold = 0.1))
#' grouping_vector(derive_split_constraints(g_kin, mode = "relatedness"))
#'
#' meta2 <- data.frame(sample_id = c("S5", "S6"), subject_id = c("P3", "P4"))
#' g_all <- combine_graphs(g, graph_from_metadata(meta2))
#' summary(g_all)$n_nodes
#' @name graph_edit
#' @export
subset_graph <- function(graph, samples, graph_name = NULL, dataset_name = NULL, validate = TRUE) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  .depgraph_assert(!is.null(samples) && length(samples) > 0L, "`samples` must name at least one sample.")

  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  keep_samples <- .depgraph_resolve_sample_node_ids(graph, samples)
  all_samples <- node_data$node_id[node_data$node_type == "Sample"]
  dropped_samples <- setdiff(all_samples, keep_samples)

  # Edges rooted at kept samples; a sample-sample edge needs both ends kept.
  sample_rooted <- edge_data$from %in% keep_samples & !(edge_data$to %in% dropped_samples)
  kept_nodes <- unique(c(keep_samples, edge_data$to[sample_rooted]))

  # Transitive closure over non-sample edges, except timepoint_precedes.
  non_sample_edge <- !(edge_data$from %in% all_samples) & !(edge_data$to %in% all_samples) &
    edge_data$edge_type != "timepoint_precedes"
  repeat {
    reach <- non_sample_edge & edge_data$from %in% kept_nodes & !(edge_data$to %in% kept_nodes)
    if (!any(reach)) break
    kept_nodes <- unique(c(kept_nodes, edge_data$to[reach]))
  }
  # Undirected-in-spirit relations (subject_related_to) can also reach a kept
  # node from their `to` side; include those neighbours once as well.
  undirected <- edge_data$edge_type == "subject_related_to" & edge_data$to %in% kept_nodes & !(edge_data$from %in% kept_nodes)
  kept_nodes <- unique(c(kept_nodes, edge_data$from[undirected]))

  keep_edge <- sample_rooted |
    (non_sample_edge & edge_data$from %in% kept_nodes & edge_data$to %in% kept_nodes) |
    (edge_data$edge_type == "timepoint_precedes" & edge_data$from %in% kept_nodes & edge_data$to %in% kept_nodes)

  new_nodes <- node_data[node_data$node_id %in% kept_nodes, , drop = FALSE]
  new_edges <- edge_data[keep_edge, , drop = FALSE]
  row.names(new_nodes) <- NULL
  row.names(new_edges) <- NULL

  .depgraph_rebuild(
    new_nodes, new_edges, graph$metadata,
    graph_name = graph_name %||% graph$metadata$graph_name,
    dataset_name = dataset_name %||% graph$metadata$dataset_name,
    validate = validate,
    extra_metadata = list(
      subset_of = graph$metadata$graph_name,
      n_samples_before_subset = length(all_samples)
    )
  )
}

#' @rdname graph_edit
#' @export
combine_graphs <- function(..., graph_name = NULL, dataset_name = NULL, validate = TRUE) {
  graphs <- list(...)
  if (length(graphs) == 1L && is.list(graphs[[1L]]) && !inherits(graphs[[1L]], "dependency_graph")) {
    graphs <- graphs[[1L]]
  }
  .depgraph_assert(length(graphs) >= 2L, "`combine_graphs()` needs at least two graphs.")
  for (g in graphs) {
    .depgraph_assert(inherits(g, "dependency_graph"), "Every argument to `combine_graphs()` must be a `dependency_graph`.")
  }

  node_data <- .depgraph_dedupe_nodes(do.call(rbind, lapply(graphs, function(g) g$nodes$data)))
  edge_data <- .depgraph_dedupe_edges(do.call(rbind, lapply(graphs, function(g) g$edges$data)))
  edge_data <- .depgraph_renumber_edge_ids(edge_data)

  merged_overrides <- list()
  merged_sources <- list()
  first_name <- NULL
  first_dataset <- NULL
  for (g in graphs) {
    merged_overrides <- utils::modifyList(merged_overrides, g$metadata$validation_overrides %||% list())
    merged_sources <- utils::modifyList(merged_sources, g$metadata$edge_sources %||% list())
    if (is.null(first_name)) first_name <- g$metadata$graph_name
    if (is.null(first_dataset)) first_dataset <- g$metadata$dataset_name
  }

  .depgraph_rebuild(
    node_data, edge_data,
    metadata = list(validation_overrides = merged_overrides, edge_sources = merged_sources),
    graph_name = graph_name %||% first_name,
    dataset_name = dataset_name %||% first_dataset,
    validate = validate,
    extra_metadata = list(
      combined_from = vapply(graphs, function(g) g$metadata$graph_name %||% NA_character_, character(1))
    )
  )
}

#' @rdname graph_edit
#' @export
add_edges <- function(graph, edges, graph_name = NULL, dataset_name = NULL, validate = TRUE) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  if (inherits(edges, "graph_edge_set")) edges <- list(edges)
  .depgraph_assert(
    is.list(edges) && length(edges) > 0L && all(vapply(edges, inherits, logical(1), what = "graph_edge_set")),
    "`edges` must be a `graph_edge_set` or a list of them."
  )

  existing <- graph$edges$data
  incoming <- .depgraph_bind_data(edges, "data")
  # An empty edge set is legitimate (e.g. no pair passed a threshold): the
  # graph is unchanged except for the recorded provenance.
  incoming$edge_id <- rep(NA_character_, nrow(incoming))
  combined <- .depgraph_dedupe_edges(rbind(existing, incoming))
  # Rows that survived dedupe but came from `incoming` have NA ids; number them
  # after the highest existing index of their type.
  combined <- .depgraph_renumber_edge_ids(combined, reserve = existing$edge_id)

  added_sources <- .depgraph_collect_edge_sources(edges)
  metadata <- graph$metadata
  metadata$edge_sources <- utils::modifyList(metadata$edge_sources %||% list(), added_sources)

  out <- .depgraph_rebuild(
    graph$nodes$data, combined, metadata,
    graph_name = graph_name %||% graph$metadata$graph_name,
    dataset_name = dataset_name %||% graph$metadata$dataset_name,
    validate = validate
  )
  # `.depgraph_rebuild()` keeps provenance only for relations that have edges;
  # an edge set that added nothing (no pair passed its threshold) must still
  # leave its threshold on record, so re-attach the sources just added.
  out$metadata$edge_sources <- utils::modifyList(out$metadata$edge_sources %||% list(), added_sources)
  out
}

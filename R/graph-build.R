# Graph assembly helpers.

#' Assemble and Validate Dependency Graphs
#'
#' Combine canonical node and edge tables into a typed dependency graph and
#' perform structural, semantic, and graph-local leakage-aware validation.
#'
#' @param nodes,edges Lists of \code{graph_node_set} and \code{graph_edge_set}
#'   objects.
#' @param graph_name,dataset_name Optional metadata labels.
#' @param validate If \code{TRUE}, run \code{validate_graph()} before returning.
#' @param validation_overrides Optional named list of explicit validation
#'   exceptions.
#' @param graph A \code{dependency_graph}.
#' @param checks Validation checks to run.
#' @param error_on_fail If \code{TRUE}, stop when validation errors are found
#'   across all detected issues from the selected validation levels, even if
#'   those errors are hidden from \code{issues} by \code{severities}.
#' @param levels Optional validation layers to run.
#' @param severities Optional severities to retain in the returned
#'   \code{issues} table. This filter does not change whether the graph is
#'   considered valid.
#' @param x A \code{dependency_graph}.
#' @return For \code{build_dependency_graph()}, a \code{dependency_graph}. For
#'   \code{validate_graph()} and \code{validate_depgraph()}, a
#'   \code{depgraph_validation_report}. For \code{as_igraph()}, the underlying
#'   \code{igraph} object.
#' @examples
#' meta <- data.frame(
#'   sample_id = c("S1", "S2"),
#'   subject_id = c("P1", "P2")
#' )
#'
#' samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
#' subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
#' edges <- create_edges(
#'   meta,
#'   "sample_id",
#'   "subject_id",
#'   "Sample",
#'   "Subject",
#'   "sample_belongs_to_subject"
#' )
#'
#' g <- build_dependency_graph(list(samples, subjects), list(edges))
#' validate_graph(g)
#' @export
build_dependency_graph <- function(nodes, edges, graph_name = NULL, dataset_name = NULL, validate = TRUE, validation_overrides = list()) {
  node_data <- .depgraph_bind_data(nodes, "data")
  edge_data <- .depgraph_bind_data(edges, "data")

  .depgraph_assert(any(node_data$node_type == "Sample"), "A dependency graph must contain at least one `Sample` node.")
  .depgraph_assert(anyDuplicated(node_data$node_id) == 0L, "Duplicate `node_id` values found in node sets.")
  .depgraph_assert(anyDuplicated(edge_data$edge_id) == 0L, "Duplicate `edge_id` values found in edge sets.")
  .depgraph_assert(
    all(edge_data$from %in% node_data$node_id),
    "Edge data contains `from` references that are not present in the node table."
  )
  .depgraph_assert(
    all(edge_data$to %in% node_data$node_id),
    "Edge data contains `to` references that are not present in the node table."
  )

  graph_obj <- dependency_graph(
    nodes = graph_node_set(node_data),
    edges = graph_edge_set(edge_data),
    graph = NULL,
    metadata = list(
      graph_name = graph_name,
      dataset_name = dataset_name,
      created_at = Sys.time(),
      schema_version = .depgraph_schema_version,
      validation_overrides = validation_overrides
    )
  )

  if (isTRUE(validate)) {
    validation <- validate_graph(graph_obj)
    if (!isTRUE(validation$valid)) {
      stop(
        paste(
          c("Graph validation failed.", validation$errors, validation$warnings),
          collapse = "\n"
        ),
        call. = FALSE
      )
    }
  }

  graph_obj
}

#' @rdname build_dependency_graph
#' @export
build_depgraph <- function(nodes, edges, graph_name = NULL, dataset_name = NULL, validate = TRUE, validation_overrides = list()) {
  build_dependency_graph(
    nodes = nodes,
    edges = edges,
    graph_name = graph_name,
    dataset_name = dataset_name,
    validate = validate,
    validation_overrides = validation_overrides
  )
}

#' @rdname build_dependency_graph
#' @export
as_igraph <- function(x) {
  .depgraph_assert(inherits(x, "dependency_graph"), "`x` must be a `dependency_graph`.")
  x$graph
}

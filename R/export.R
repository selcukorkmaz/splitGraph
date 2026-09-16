# Export a dependency_graph to formats other tools read (GraphML, GML, CSV).

# Flatten a list-column of named attribute lists into typed columns named
# "attr_<name>". A value of length > 1 is collapsed with ";"; a column whose
# values are all numeric (or all logical) keeps that type, anything else
# becomes character. GraphML/GML attributes must be scalars, and CSV has no
# nesting, so this is the only faithful representation available.
.depgraph_flatten_attrs <- function(attrs, prefix = "attr_") {
  n <- length(attrs)
  attr_names <- unique(unlist(lapply(attrs, names), use.names = FALSE))
  if (length(attr_names) == 0L) {
    return(data.frame(row.names = seq_len(n))[, FALSE, drop = FALSE])
  }
  columns <- lapply(attr_names, function(name) {
    values <- lapply(attrs, function(a) a[[name]])
    scalar <- lengths(values) <= 1L
    values[!scalar] <- lapply(values[!scalar], function(v) paste(as.character(v), collapse = ";"))
    values[lengths(values) == 0L] <- list(NA)
    flat <- unlist(lapply(values, function(v) v[[1L]]), use.names = FALSE)
    if (is.numeric(flat) || is.logical(flat)) flat else as.character(flat)
  })
  names(columns) <- paste0(prefix, attr_names)
  as.data.frame(columns, stringsAsFactors = FALSE, optional = TRUE)
}

.depgraph_export_tables <- function(graph) {
  nodes <- graph$nodes$data
  edges <- graph$edges$data
  node_table <- cbind(
    nodes[, c("node_id", "node_type", "node_key", "label"), drop = FALSE],
    .depgraph_flatten_attrs(nodes$attrs)
  )
  edge_table <- cbind(
    edges[, c("from", "to", "edge_id", "edge_type"), drop = FALSE],
    .depgraph_flatten_attrs(edges$attrs)
  )
  row.names(node_table) <- NULL
  row.names(edge_table) <- NULL
  list(nodes = node_table, edges = edge_table)
}

#' Export a Dependency Graph for Other Tools
#'
#' Write a \code{dependency_graph} as GraphML or GML (readable by Cytoscape,
#' Gephi, networkx, and \code{igraph::read_graph()}), or as flat CSV tables of
#' nodes or edges. This complements the lossless JSON format of
#' \code{\link{write_dependency_graph}}: the JSON round-trips exactly and is
#' the interchange contract; these exports are for visual inspection and
#' analysis in graph tools and lose nothing but the nesting of attributes.
#'
#' Node attributes (the \code{attrs} list-column) are flattened into scalar
#' columns named \code{attr_<name>}; multi-valued attributes are collapsed with
#' \code{";"}. Canonical columns (\code{node_id}, \code{node_type},
#' \code{node_key}, \code{label}; \code{edge_id}, \code{edge_type}) are written
#' as-is. In GraphML/GML the vertex \code{name} is the \code{node_id}.
#'
#' @param graph A \code{dependency_graph}.
#' @param file Path to write. For the CSV formats this is the single table
#'   requested (\code{nodes_csv} writes the node table, \code{edges_csv} the
#'   edge table).
#' @param format One of \code{"graphml"}, \code{"gml"}, \code{"nodes_csv"},
#'   \code{"edges_csv"}.
#' @return The normalised output path, invisibly.
#' @examples
#' meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"))
#' g <- graph_from_metadata(meta)
#' tmp <- tempfile(fileext = ".graphml")
#' export_graph(g, tmp, format = "graphml")
#' igraph::vcount(igraph::read_graph(tmp, format = "graphml"))
#' unlink(tmp)
#' @export
export_graph <- function(graph, file, format = c("graphml", "gml", "nodes_csv", "edges_csv")) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  .depgraph_assert(is.character(file) && length(file) == 1L && nzchar(file), "`file` must be a single non-empty file path.")
  format <- .depgraph_match_arg(format, c("graphml", "gml", "nodes_csv", "edges_csv"), "format")
  .depgraph_check_writable_path(file)

  tables <- .depgraph_export_tables(graph)

  if (identical(format, "nodes_csv")) {
    utils::write.csv(tables$nodes, file, row.names = FALSE, na = "")
  } else if (identical(format, "edges_csv")) {
    utils::write.csv(tables$edges, file, row.names = FALSE, na = "")
  } else {
    vertices <- tables$nodes
    names(vertices)[names(vertices) == "node_id"] <- "name"
    g <- igraph::graph_from_data_frame(tables$edges, vertices = vertices, directed = TRUE)
    igraph::write_graph(g, file, format = format)
  }

  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}

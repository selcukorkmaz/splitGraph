# Query helpers for dependency_graph objects.

# Resolve the user-supplied `max_length` to an `igraph::all_simple_paths`
# `cutoff` value. NULL -> documented default cap; Inf -> no cap (cutoff=-1);
# any non-negative integer is passed through. Any other input errors.
.depgraph_resolve_path_cutoff <- function(max_length) {
  if (is.null(max_length)) {
    return(.depgraph_default_path_cap)
  }
  if (length(max_length) != 1L || !is.numeric(max_length)) {
    .depgraph_stop("`max_length` must be a single numeric value, Inf, or NULL.")
  }
  if (is.infinite(max_length)) {
    return(-1)
  }
  if (is.na(max_length) || max_length < 0) {
    .depgraph_stop("`max_length` must be non-negative.")
  }
  as.integer(max_length)
}

.depgraph_resolve_node_ids <- function(graph, node_ids) {
  node_data <- graph$nodes$data
  node_ids <- unique(as.character(node_ids))
  .depgraph_assert(length(node_ids) > 0L, "`node_ids` must contain at least one value.")
  .depgraph_assert(
    all(node_ids %in% node_data$node_id),
    paste0("Unknown node IDs: ", paste(setdiff(node_ids, node_data$node_id), collapse = ", ")),
    class = "splitgraph_reference_error", code = "unknown_node_ids"
  )
  node_ids
}

.depgraph_resolve_sample_node_ids <- function(graph, samples = NULL) {
  sample_nodes <- graph$nodes$data[graph$nodes$data$node_type == "Sample", , drop = FALSE]
  if (is.null(samples)) {
    return(sample_nodes$node_id)
  }

  samples <- unique(as.character(samples))
  valid_inputs <- unique(c(sample_nodes$node_id, sample_nodes$node_key))
  unknown <- setdiff(samples, valid_inputs)
  .depgraph_assert(
    length(unknown) == 0L,
    paste0("Unknown sample IDs: ", paste(unknown, collapse = ", ")),
    class = "splitgraph_reference_error", code = "unknown_sample_ids"
  )

  matched <- sample_nodes$node_id[sample_nodes$node_id %in% samples]
  matched <- unique(c(
    matched,
    sample_nodes$node_id[match(samples, sample_nodes$node_key, nomatch = 0L)]
  ))
  matched <- matched[!is.na(matched) & nzchar(matched)]

  .depgraph_assert(length(matched) > 0L, "No matching sample nodes found.")
  matched
}

.depgraph_subset_edges <- function(graph, edge_types = NULL, node_ids = NULL) {
  edge_data <- graph$edges$data

  if (!is.null(edge_types)) {
    edge_types <- unique(as.character(edge_types))
    edge_data <- edge_data[edge_data$edge_type %in% edge_types, , drop = FALSE]
  }

  if (!is.null(node_ids)) {
    node_ids <- unique(as.character(node_ids))
    edge_data <- edge_data[edge_data$from %in% node_ids | edge_data$to %in% node_ids, , drop = FALSE]
  }

  edge_data
}

.depgraph_query_result <- function(graph, query, params, node_ids = NULL, edge_ids = NULL, table = NULL, metadata = list()) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data

  nodes <- if (is.null(node_ids)) {
    node_data[0, , drop = FALSE]
  } else {
    node_data[node_data$node_id %in% unique(node_ids), , drop = FALSE]
  }

  edges <- if (is.null(edge_ids)) {
    edge_data[0, , drop = FALSE]
  } else {
    edge_data[edge_data$edge_id %in% unique(edge_ids), , drop = FALSE]
  }

  graph_query_result(
    query = query,
    params = params,
    nodes = nodes,
    edges = edges,
    table = if (is.null(table)) data.frame(stringsAsFactors = FALSE) else table,
    metadata = metadata
  )
}

.depgraph_filter_graph_by_edge_type <- function(graph, edge_types = NULL) {
  if (is.null(edge_types)) {
    return(graph$graph)
  }

  edge_types <- unique(as.character(edge_types))
  keep_ids <- graph$edges$data$edge_id[graph$edges$data$edge_type %in% edge_types]
  keep_edges <- igraph::E(graph$graph)[igraph::E(graph$graph)$edge_id %in% keep_ids]
  igraph::subgraph_from_edges(graph$graph, eids = keep_edges, delete.vertices = FALSE)
}

.depgraph_edge_between <- function(edge_data, from, to, undirected = FALSE) {
  matches <- edge_data$from == from & edge_data$to == to
  if (isTRUE(undirected)) {
    matches <- matches | (edge_data$from == to & edge_data$to == from)
  }
  edge_data[matches, , drop = FALSE]
}

.depgraph_format_paths <- function(graph, path_node_ids, edge_data, query_name, params, metadata = list()) {
  node_data <- graph$nodes$data
  rows <- list()
  used_node_ids <- character()
  used_edge_ids <- character()

  if (length(path_node_ids) == 0L) {
    return(.depgraph_query_result(graph, query_name, params, table = data.frame(
      path_id = character(),
      step = integer(),
      node_id = character(),
      node_type = character(),
      edge_id = character(),
      edge_type = character(),
      stringsAsFactors = FALSE
    ), metadata = metadata))
  }

  row_idx <- 1L
  for (i in seq_along(path_node_ids)) {
    nodes_in_path <- path_node_ids[[i]]
    if (length(nodes_in_path) == 0L) {
      next
    }

    for (step_idx in seq_along(nodes_in_path)) {
      node_id <- nodes_in_path[[step_idx]]
      node_row <- node_data[node_data$node_id == node_id, , drop = FALSE]
      edge_id <- NA_character_
      edge_type <- NA_character_

      if (step_idx > 1L) {
        prev_node <- nodes_in_path[[step_idx - 1L]]
        edge_row <- .depgraph_edge_between(edge_data, prev_node, node_id, undirected = identical(params$mode, "all"))
        if (nrow(edge_row) > 0L) {
          edge_id <- edge_row$edge_id[[1L]]
          edge_type <- edge_row$edge_type[[1L]]
          used_edge_ids <- c(used_edge_ids, edge_id)
        }
      }

      rows[[row_idx]] <- data.frame(
        path_id = paste0("path_", i),
        step = step_idx,
        node_id = node_id,
        node_type = node_row$node_type[[1L]],
        edge_id = edge_id,
        edge_type = edge_type,
        stringsAsFactors = FALSE
      )
      used_node_ids <- c(used_node_ids, node_id)
      row_idx <- row_idx + 1L
    }
  }

  table <- do.call(rbind, rows)
  row.names(table) <- NULL

  .depgraph_query_result(
    graph = graph,
    query = query_name,
    params = params,
    node_ids = unique(used_node_ids),
    edge_ids = unique(used_edge_ids),
    table = table,
    metadata = metadata
  )
}

.depgraph_edge_types_for_via <- function(via, edge_types = NULL) {
  if (!is.null(edge_types)) {
    return(unique(as.character(edge_types)))
  }

  via <- unique(vapply(via, .depgraph_match_node_type, character(1), USE.NAMES = FALSE))
  .depgraph_edge_schema$edge_type[
    .depgraph_edge_schema$from_type == "Sample" &
      .depgraph_edge_schema$to_type %in% via
  ]
}

.depgraph_empty_shared_table <- function() {
  data.frame(
    sample_id_1 = character(),
    sample_id_2 = character(),
    sample_node_id_1 = character(),
    sample_node_id_2 = character(),
    shared_node_id = character(),
    shared_node_type = character(),
    edge_type = character(),
    stringsAsFactors = FALSE
  )
}

.depgraph_empty_projection <- function() {
  data.frame(
    sample_node_id_1 = character(),
    sample_node_id_2 = character(),
    projection_edge_id = character(),
    stringsAsFactors = FALSE
  )
}

# Sample -> dependency-target edges of the selected relation types, restricted to
# the given sample node ids. Shared by the pair table and the component search.
.depgraph_dependency_edges <- function(graph, sample_node_ids, edge_types) {
  edge_data <- graph$edges$data
  edges <- edge_data[
    edge_data$edge_type %in% edge_types & edge_data$from %in% sample_node_ids,
    c("from", "to", "edge_type"),
    drop = FALSE
  ]
  edges <- unique(edges)
  row.names(edges) <- NULL
  edges
}

# One row per unordered sample pair that shares a dependency target through one
# relation type. Vectorised as a self-merge on (target, edge_type); the number of
# rows is inherently sum over targets of choose(k, 2), so callers that only need
# the *grouping* should use `.depgraph_sample_components()` instead.
.depgraph_shared_dependency_table <- function(graph, via, samples = NULL, edge_types = NULL) {
  node_data <- graph$nodes$data
  sample_nodes <- .depgraph_resolve_sample_node_ids(graph, samples)
  selected_edge_types <- .depgraph_edge_types_for_via(via, edge_types)

  edges <- .depgraph_dependency_edges(graph, sample_nodes, selected_edge_types)
  if (nrow(edges) == 0L) {
    return(.depgraph_empty_shared_table())
  }

  # Only targets linked to at least two samples can produce a pair.
  target_key <- paste(edges$edge_type, edges$to, sep = "\r")
  multi <- target_key %in% target_key[duplicated(target_key)]
  edges <- edges[multi, , drop = FALSE]
  if (nrow(edges) == 0L) {
    return(.depgraph_empty_shared_table())
  }

  pairs <- merge(edges, edges, by = c("to", "edge_type"), suffixes = c("_1", "_2"))
  pairs <- pairs[pairs$from_1 < pairs$from_2, , drop = FALSE]
  if (nrow(pairs) == 0L) {
    return(.depgraph_empty_shared_table())
  }

  key_map <- stats::setNames(node_data$node_key, node_data$node_id)
  type_map <- stats::setNames(node_data$node_type, node_data$node_id)

  out <- data.frame(
    sample_id_1 = unname(key_map[pairs$from_1]),
    sample_id_2 = unname(key_map[pairs$from_2]),
    sample_node_id_1 = pairs$from_1,
    sample_node_id_2 = pairs$from_2,
    shared_node_id = pairs$to,
    shared_node_type = unname(type_map[pairs$to]),
    edge_type = pairs$edge_type,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$edge_type, out$shared_node_id, out$sample_node_id_1, out$sample_node_id_2), , drop = FALSE]
  row.names(out) <- NULL
  out
}

.depgraph_project_sample_dependencies <- function(shared) {
  if (nrow(shared) == 0L) {
    return(.depgraph_empty_projection())
  }

  projection <- unique(shared[, c("sample_node_id_1", "sample_node_id_2"), drop = FALSE])
  row.names(projection) <- NULL
  projection$projection_edge_id <- paste0("projection_", seq_len(nrow(projection)))
  projection
}

# Edge ids that participate in the shared-dependency table: every edge of a
# listed (edge_type, target) whose source sample appears in some pair. Because a
# sample linked to a multi-sample target always appears in a pair for that
# target, this equals "all edges into targets that produced pairs".
.depgraph_shared_dependency_edge_ids <- function(graph, shared_table) {
  if (nrow(shared_table) == 0L) {
    return(character())
  }

  edge_data <- graph$edges$data
  involved_samples <- unique(c(shared_table$sample_node_id_1, shared_table$sample_node_id_2))
  target_keys <- unique(paste(shared_table$edge_type, shared_table$shared_node_id, sep = "\r"))
  edge_keys <- paste(edge_data$edge_type, edge_data$to, sep = "\r")
  keep <- edge_keys %in% target_keys & edge_data$from %in% involved_samples
  unique(edge_data$edge_id[keep])
}

# Connected components of samples linked through shared targets, computed on the
# bipartite sample-target graph (plus any extra undirected edges, e.g.
# subject_related_to or sample_adjacent_to). Linear in nodes + edges; never
# enumerates sample pairs. Component numbers follow first appearance in
# `sample_node_ids`, matching the numbering igraph produced for the old
# projection graph.
.depgraph_sample_components <- function(sample_node_ids, edges) {
  n <- length(sample_node_ids)
  if (n == 0L) {
    return(list(membership = integer(), size = integer()))
  }

  if (nrow(edges) == 0L) {
    membership <- seq_len(n)
  } else {
    vertices <- unique(c(sample_node_ids, edges$from, edges$to))
    g <- igraph::graph_from_data_frame(
      d = data.frame(from = edges$from, to = edges$to, stringsAsFactors = FALSE),
      vertices = data.frame(name = vertices, stringsAsFactors = FALSE),
      directed = FALSE
    )
    raw <- igraph::components(g)$membership[sample_node_ids]
    membership <- match(raw, unique(raw))
  }

  list(
    membership = as.integer(membership),
    size = as.integer(tabulate(membership)[membership])
  )
}

#' Query Dependency Graph Structure
#'
#' Query graph neighborhoods, typed nodes and edges, path structure, projected
#' sample dependency components, and direct shared dependencies within a
#' \code{dependency_graph}.
#'
#' When a \code{samples} subset is supplied, partial matching is not allowed:
#' unknown sample identifiers raise an error rather than being silently
#' dropped.
#'
#' @param graph A \code{dependency_graph}.
#' @param node_ids,from,to Node identifiers to use as query seeds or endpoints.
#' @param edge_types Optional edge types used to filter the traversal graph or
#'   edge table.
#' @param node_types Optional node types used to filter node results or allowed
#'   path members.
#' @param direction,mode Traversal direction.
#' @param ids Optional node identifiers used to further restrict
#'   \code{query_node_type()}.
#' @param max_length Maximum path length (number of edges) for
#'   \code{query_paths()}. Defaults to a documented finite cap
#'   (\code{8}) so that \code{igraph::all_simple_paths()} cannot blow up
#'   on dense graphs. Pass \code{Inf} to opt out and search exhaustively;
#'   pass any non-negative integer for an explicit cap. Negative values and
#'   non-numeric inputs are rejected.
#' @param via Dependency node types used for sample-level dependency detection.
#' @param min_size Minimum component size retained by
#'   \code{detect_dependency_components()}.
#' @param samples Optional sample identifiers or sample node IDs used to
#'   restrict direct shared-dependency detection. All requested samples must
#'   resolve successfully.
#' @return Each function returns a \code{graph_query_result}. Use
#'   \code{as.data.frame()} to obtain the tidy result table.
#' @examples
#' meta <- data.frame(
#'   sample_id  = c("S1", "S2", "S3"),
#'   subject_id = c("P1", "P1", "P2"),
#'   batch_id   = c("B1", "B2", "B1")
#' )
#' g <- graph_from_metadata(meta)
#'
#' query_node_type(g, "Sample")
#' query_neighbors(g, "sample:S1", direction = "out")
#' detect_shared_dependencies(g, via = "Subject")
#' @export
query_node_type <- function(graph, node_types, ids = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  node_types <- unique(vapply(node_types, .depgraph_match_node_type, character(1), USE.NAMES = FALSE))

  node_data <- graph$nodes$data
  table <- node_data[node_data$node_type %in% node_types, , drop = FALSE]
  if (!is.null(ids)) {
    ids <- unique(as.character(ids))
    table <- table[table$node_id %in% ids, , drop = FALSE]
  }

  .depgraph_query_result(
    graph = graph,
    query = "query_node_type",
    params = list(node_types = node_types, ids = ids),
    node_ids = table$node_id,
    table = table
  )
}

#' @rdname query_node_type
#' @export
query_edge_type <- function(graph, edge_types, node_ids = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  edge_types <- unique(as.character(edge_types))
  if (!is.null(node_ids)) {
    node_ids <- .depgraph_resolve_node_ids(graph, node_ids)
  }

  table <- .depgraph_subset_edges(graph, edge_types = edge_types, node_ids = node_ids)
  touched_nodes <- unique(c(table$from, table$to))

  .depgraph_query_result(
    graph = graph,
    query = "query_edge_type",
    params = list(edge_types = edge_types, node_ids = node_ids),
    node_ids = touched_nodes,
    edge_ids = table$edge_id,
    table = table
  )
}

#' @rdname query_node_type
#' @export
query_neighbors <- function(graph, node_ids, edge_types = NULL, node_types = NULL, direction = c("out", "in", "all")) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  direction <- .depgraph_match_arg(direction, c("out", "in", "all"), "direction")
  seed_ids <- .depgraph_resolve_node_ids(graph, node_ids)
  g <- .depgraph_filter_graph_by_edge_type(graph, edge_types)

  rows <- list()
  row_idx <- 1L
  used_nodes <- character()
  used_edges <- character()
  node_data <- graph$nodes$data
  edge_data <- .depgraph_subset_edges(graph, edge_types = edge_types)

  for (seed in seed_ids) {
    neigh <- igraph::neighbors(g, v = seed, mode = direction)
    neigh_ids <- igraph::as_ids(neigh)
    if (!is.null(node_types) && length(neigh_ids) > 0L) {
      node_types <- unique(vapply(node_types, .depgraph_match_node_type, character(1), USE.NAMES = FALSE))
      keep <- node_data$node_type[match(neigh_ids, node_data$node_id)] %in% node_types
      neigh_ids <- neigh_ids[keep]
    }

    for (neighbor in neigh_ids) {
      edge_rows <- if (identical(direction, "out")) {
        edge_data[edge_data$from == seed & edge_data$to == neighbor, , drop = FALSE]
      } else if (identical(direction, "in")) {
        edge_data[edge_data$from == neighbor & edge_data$to == seed, , drop = FALSE]
      } else {
        .depgraph_edge_between(edge_data, seed, neighbor, undirected = TRUE)
      }

      if (nrow(edge_rows) == 0L) {
        next
      }

      neighbor_row <- node_data[node_data$node_id == neighbor, , drop = FALSE]
      for (j in seq_len(nrow(edge_rows))) {
        rows[[row_idx]] <- data.frame(
          seed_node_id = seed,
          node_id = neighbor,
          node_type = neighbor_row$node_type[[1L]],
          edge_id = edge_rows$edge_id[[j]],
          edge_type = edge_rows$edge_type[[j]],
          direction = direction,
          stringsAsFactors = FALSE
        )
        used_nodes <- c(used_nodes, seed, neighbor)
        used_edges <- c(used_edges, edge_rows$edge_id[[j]])
        row_idx <- row_idx + 1L
      }
    }
  }

  table <- if (length(rows) == 0L) {
    data.frame(
      seed_node_id = character(),
      node_id = character(),
      node_type = character(),
      edge_id = character(),
      edge_type = character(),
      direction = character(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    row.names(out) <- NULL
    out
  }

  .depgraph_query_result(
    graph = graph,
    query = "query_neighbors",
    params = list(node_ids = seed_ids, edge_types = edge_types, node_types = node_types, direction = direction),
    node_ids = unique(used_nodes),
    edge_ids = unique(used_edges),
    table = table
  )
}

#' @rdname query_node_type
#' @export
query_paths <- function(graph, from, to, edge_types = NULL, node_types = NULL, mode = c("out", "in", "all"), max_length = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  mode <- .depgraph_match_arg(mode, c("out", "in", "all"), "mode")
  from_ids <- .depgraph_resolve_node_ids(graph, from)
  to_ids <- .depgraph_resolve_node_ids(graph, to)
  cutoff_value <- .depgraph_resolve_path_cutoff(max_length)
  filtered_graph <- .depgraph_filter_graph_by_edge_type(graph, edge_types)
  edge_data <- .depgraph_subset_edges(graph, edge_types = edge_types)
  path_nodes <- list()
  path_idx <- 1L
  # Truncation tracking: TRUE whenever a returned path reaches the cap in
  # edges, which means longer simple paths between the same endpoints may
  # have been suppressed. Conservative — never a false negative when a
  # finite cap was binding, may flag false positives (returned paths at the
  # boundary that happen to have no longer siblings).
  cap_binding <- (cutoff_value != -1L)
  hit_cap <- FALSE

  for (from_id in from_ids) {
    for (to_id in to_ids) {
      paths <- igraph::all_simple_paths(
        filtered_graph,
        from = from_id,
        to = to_id,
        mode = mode,
        cutoff = cutoff_value
      )
      if (length(paths) == 0L) {
        next
      }

      for (path in paths) {
        node_ids_in_path <- igraph::as_ids(path)
        if (!is.null(node_types)) {
          allowed_types <- unique(vapply(node_types, .depgraph_match_node_type, character(1), USE.NAMES = FALSE))
          path_types <- graph$nodes$data$node_type[match(node_ids_in_path, graph$nodes$data$node_id)]
          if (!all(path_types %in% allowed_types)) {
            next
          }
        }
        if (cap_binding && (length(node_ids_in_path) - 1L) >= cutoff_value) {
          hit_cap <- TRUE
        }
        path_nodes[[path_idx]] <- node_ids_in_path
        path_idx <- path_idx + 1L
      }
    }
  }

  .depgraph_format_paths(
    graph = graph,
    path_node_ids = path_nodes,
    edge_data = edge_data,
    query_name = "query_paths",
    params = list(from = from_ids, to = to_ids, edge_types = edge_types, node_types = node_types, mode = mode, max_length = max_length),
    metadata = list(
      truncated = isTRUE(cap_binding && hit_cap),
      max_length = if (cap_binding) as.integer(cutoff_value) else Inf
    )
  )
}

#' @rdname query_node_type
#' @export
query_shortest_paths <- function(graph, from, to, edge_types = NULL, node_types = NULL, mode = c("out", "in", "all")) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  mode <- .depgraph_match_arg(mode, c("out", "in", "all"), "mode")
  from_ids <- .depgraph_resolve_node_ids(graph, from)
  to_ids <- .depgraph_resolve_node_ids(graph, to)
  filtered_graph <- .depgraph_filter_graph_by_edge_type(graph, edge_types)
  edge_data <- .depgraph_subset_edges(graph, edge_types = edge_types)
  allowed_types <- NULL
  if (!is.null(node_types)) {
    allowed_types <- unique(vapply(node_types, .depgraph_match_node_type, character(1), USE.NAMES = FALSE))
    allowed_nodes <- graph$nodes$data$node_id[graph$nodes$data$node_type %in% allowed_types]
    filtered_graph <- igraph::induced_subgraph(filtered_graph, vids = allowed_nodes)
  }
  path_nodes <- list()
  path_idx <- 1L

  for (from_id in from_ids) {
    for (to_id in to_ids) {
      graph_nodes <- igraph::V(filtered_graph)$name
      if (!from_id %in% graph_nodes || !to_id %in% graph_nodes) {
        next
      }
      shortest <- igraph::shortest_paths(filtered_graph, from = from_id, to = to_id, mode = mode)$vpath
      if (length(shortest) == 0L || length(shortest[[1L]]) == 0L) {
        next
      }
      node_ids_in_path <- igraph::as_ids(shortest[[1L]])
      if (!is.null(allowed_types)) {
        path_types <- graph$nodes$data$node_type[match(node_ids_in_path, graph$nodes$data$node_id)]
        if (!all(path_types %in% allowed_types)) {
          next
        }
      }
      path_nodes[[path_idx]] <- node_ids_in_path
      path_idx <- path_idx + 1L
    }
  }

  .depgraph_format_paths(
    graph = graph,
    path_node_ids = path_nodes,
    edge_data = edge_data,
    query_name = "query_shortest_paths",
    params = list(from = from_ids, to = to_ids, edge_types = edge_types, node_types = node_types, mode = mode)
  )
}

#' @rdname query_node_type
#' @export
detect_dependency_components <- function(graph,
                                         via = c("Subject", "Batch", "Study", "Timepoint",
                                                 "Assay", "FeatureSet", "Outcome"),
                                         edge_types = NULL, min_size = 1) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  sample_nodes <- graph$nodes$data[graph$nodes$data$node_type == "Sample", , drop = FALSE]
  selected_edge_types <- .depgraph_edge_types_for_via(via, edge_types)

  # Grouping first, on the bipartite sample-target graph (linear); the explicit
  # pair table below is only materialised for the samples that survive min_size.
  dep_edges <- .depgraph_dependency_edges(graph, sample_nodes$node_id, selected_edge_types)
  comps <- .depgraph_sample_components(sample_nodes$node_id, dep_edges)
  table <- data.frame(
    sample_id = sample_nodes$node_key,
    sample_node_id = sample_nodes$node_id,
    component_id = paste0("component_", comps$membership),
    component_size = comps$size,
    stringsAsFactors = FALSE
  )

  table <- table[table$component_size >= min_size, , drop = FALSE]
  row.names(table) <- NULL
  keep_nodes <- table$sample_node_id

  shared <- if (length(keep_nodes) == 0L) {
    .depgraph_empty_shared_table()
  } else {
    .depgraph_shared_dependency_table(graph, via = via, samples = keep_nodes, edge_types = edge_types)
  }
  projection <- .depgraph_project_sample_dependencies(shared)
  edge_ids <- .depgraph_shared_dependency_edge_ids(graph, shared)

  .depgraph_query_result(
    graph = graph,
    query = "detect_dependency_components",
    params = list(via = via, edge_types = edge_types, min_size = min_size),
    node_ids = keep_nodes,
    table = table,
    metadata = list(
      n_components = length(unique(table$component_id)),
      projection_edges = projection
    ),
    edge_ids = edge_ids
  )
}

#' @rdname query_node_type
#' @export
detect_shared_dependencies <- function(graph, via = c("Subject", "Batch", "Study", "Timepoint"), samples = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  table <- .depgraph_shared_dependency_table(graph, via = via, samples = samples)

  node_ids <- unique(c(
    table$sample_node_id_1,
    table$sample_node_id_2,
    table$shared_node_id
  ))

  edge_ids <- .depgraph_shared_dependency_edge_ids(graph, table)

  .depgraph_query_result(
    graph = graph,
    query = "detect_shared_dependencies",
    params = list(via = via, samples = samples),
    node_ids = node_ids,
    edge_ids = unique(edge_ids),
    table = table
  )
}

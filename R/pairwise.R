# Pairwise (thresholded) leakage relations. Unlike the direct-assignment modes
# (subject / batch / site / ...), these relations are *pairwise and continuous*:
# leakage risk is a graded property of a pair of subjects (genetic relatedness)
# or samples (spatial proximity). They are modelled as undirected, thresholded
# edges, then collapsed into groups via transitive closure (connected
# components) over the thresholded edge set. The thresholding happens up front
# in the edge-building helpers below; the derivation modes only run the
# component search over whatever edges the graph already carries.

.depgraph_pairwise_relation <- c(
  relatedness = "subject_related_to",
  spatial     = "sample_adjacent_to"
)

# Build sample-sample projection edges for a pairwise mode, restricted to the
# in-scope sample node ids. For "spatial" the graph already carries
# sample-sample edges. For "relatedness" the graph carries subject-subject
# edges, so we expand them onto samples: two samples are linked when they share
# a subject (same individual) or when their subjects are directly related.
.depgraph_pairwise_projection_edges <- function(graph, mode, sample_node_ids) {
  relation <- .depgraph_pairwise_relation[[mode]]
  edge_data <- graph$edges$data

  empty <- data.frame(
    sample_node_id_1 = character(),
    sample_node_id_2 = character(),
    stringsAsFactors = FALSE
  )

  if (identical(mode, "spatial")) {
    e <- edge_data[
      edge_data$edge_type == relation &
        edge_data$from %in% sample_node_ids &
        edge_data$to %in% sample_node_ids,
      c("from", "to"),
      drop = FALSE
    ]
    if (nrow(e) == 0L) return(empty)
    return(unique(data.frame(
      sample_node_id_1 = e$from,
      sample_node_id_2 = e$to,
      stringsAsFactors = FALSE
    )))
  }

  # relatedness: sample -> subject for in-scope samples.
  belongs <- edge_data[
    edge_data$edge_type == "sample_belongs_to_subject" &
      edge_data$from %in% sample_node_ids,
    c("from", "to"),
    drop = FALSE
  ]
  if (nrow(belongs) == 0L) return(empty)

  samples_of_subject <- split(belongs$from, belongs$to)
  parts <- list()

  # within-subject: samples from the same individual are always grouped.
  for (subject in names(samples_of_subject)) {
    s <- unique(samples_of_subject[[subject]])
    if (length(s) >= 2L) {
      cmb <- utils::combn(s, 2L)
      parts[[length(parts) + 1L]] <- data.frame(
        sample_node_id_1 = cmb[1L, ],
        sample_node_id_2 = cmb[2L, ],
        stringsAsFactors = FALSE
      )
    }
  }

  # across related subjects (undirected subject_related_to edges).
  rel_edges <- edge_data[edge_data$edge_type == relation, c("from", "to"), drop = FALSE]
  for (i in seq_len(nrow(rel_edges))) {
    sa <- samples_of_subject[[rel_edges$from[[i]]]]
    sb <- samples_of_subject[[rel_edges$to[[i]]]]
    if (length(sa) > 0L && length(sb) > 0L) {
      grid <- expand.grid(a = sa, b = sb, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      parts[[length(parts) + 1L]] <- data.frame(
        sample_node_id_1 = grid$a,
        sample_node_id_2 = grid$b,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(parts) == 0L) return(empty)
  unique(do.call(rbind, parts))
}

.derive_pairwise_constraints <- function(graph, mode, samples = NULL) {
  relation <- .depgraph_pairwise_relation[[mode]]
  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  keep_ids <- sample_nodes$node_id

  projection <- .depgraph_pairwise_projection_edges(graph, mode, keep_ids)

  subset_graph <- if (nrow(projection) == 0L) {
    igraph::make_empty_graph(n = length(keep_ids), directed = FALSE)
  } else {
    igraph::graph_from_data_frame(
      d = data.frame(
        from = projection$sample_node_id_1,
        to = projection$sample_node_id_2,
        stringsAsFactors = FALSE
      ),
      vertices = data.frame(name = keep_ids, stringsAsFactors = FALSE),
      directed = FALSE
    )
  }
  igraph::V(subset_graph)$name <- keep_ids

  comps <- igraph::components(subset_graph)
  membership_idx <- as.integer(comps$membership[keep_ids])
  component_size <- as.integer(comps$csize[membership_idx])

  sample_map <- data.frame(
    sample_id = sample_nodes$node_key,
    sample_node_id = keep_ids,
    group_id = paste0(mode, ":component_", membership_idx),
    constraint_type = mode,
    group_label = paste0("component_", membership_idx),
    explanation = paste0(
      "Grouped by transitive closure over thresholded ", relation,
      " edges (connected component ", membership_idx, ")."
    ),
    stringsAsFactors = FALSE
  )
  row.names(sample_map) <- NULL

  warnings <- character()
  if (identical(mode, "relatedness")) {
    linked_samples <- unique(c(projection$sample_node_id_1, projection$sample_node_id_2))
    without_subject <- setdiff(
      keep_ids,
      graph$edges$data$from[graph$edges$data$edge_type == "sample_belongs_to_subject"]
    )
    missing_subject <- intersect(keep_ids, without_subject)
    if (length(missing_subject) > 0L) {
      warnings <- c(warnings, paste0(
        "Samples without a subject assignment were retained as singleton groups ",
        "(relatedness cannot be assessed): ",
        paste(sample_nodes$node_key[match(missing_subject, keep_ids)], collapse = ", ")
      ))
    }
  }
  if (nrow(sample_map) > 0L && mean(component_size == 1L) > 0.5) {
    warnings <- c(warnings, paste0(
      "Most ", mode, " groups are singletons; pairwise coverage may be sparse ",
      "or the threshold may be too strict."
    ))
  }

  split_constraint(
    strategy = mode,
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = mode,
      strategy = mode,
      relations_used = relation,
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings,
      projection_edges = projection
    )
  )
}

#' Build Pairwise Leakage Edges from Continuous Similarity
#'
#' Helpers that turn a continuous, pairwise similarity signal into the
#' thresholded, undirected edges consumed by
#' \code{derive_split_constraints(mode = "relatedness")} and
#' \code{derive_split_constraints(mode = "spatial")}. Only pairs that pass the
#' threshold become edges; the derivation modes then form groups as connected
#' components over those edges (transitive closure), so a chain of individually
#' below-radius neighbours can still land in one group.
#'
#' \code{relatedness_edges_from_kinship()} keeps subject pairs whose kinship (or
#' relatedness) coefficient is \emph{at least} \code{threshold} and emits
#' \code{subject_related_to} edges (\code{Subject} -> \code{Subject}).
#'
#' \code{spatial_edges_from_coords()} keeps sample pairs whose Euclidean
#' distance over the coordinate columns is \emph{at most} \code{radius} and
#' emits \code{sample_adjacent_to} edges (\code{Sample} -> \code{Sample}).
#'
#' Both return a \code{graph_edge_set} that can be combined with the other node
#' and edge sets in \code{build_dependency_graph()}. The passing metric value is
#' carried on each edge as an attribute (\code{kinship} / \code{distance}).
#'
#' @param pairs A data.frame of subject pairs with two id columns and a metric
#'   column.
#' @param threshold Minimum kinship value (inclusive) for a pair to be kept.
#' @param id1,id2 Column names in \code{pairs} holding the two subject ids.
#' @param kinship Column name in \code{pairs} holding the kinship / relatedness
#'   value.
#' @param coords A data.frame with one row per sample: a sample id column plus
#'   the numeric coordinate columns.
#' @param radius Maximum distance (inclusive) for two samples to be adjacent.
#' @param id Column name in \code{coords} holding the sample id.
#' @param coord_cols Character vector of coordinate columns in \code{coords}.
#'   Defaults to every numeric column other than \code{id}.
#' @return A \code{graph_edge_set}.
#' @examples
#' pairs <- data.frame(
#'   id1 = c("P1", "P1", "P2"),
#'   id2 = c("P2", "P3", "P3"),
#'   kinship = c(0.25, 0.02, 0.30)
#' )
#' relatedness_edges_from_kinship(pairs, threshold = 0.1)
#'
#' coords <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   x = c(0, 1, 9),
#'   y = c(0, 1, 9)
#' )
#' spatial_edges_from_coords(coords, radius = 2)
#' @name pairwise_edges
#' @export
relatedness_edges_from_kinship <- function(pairs, threshold, id1 = "id1", id2 = "id2", kinship = "kinship") {
  .depgraph_assert(is.data.frame(pairs), "`pairs` must be a data.frame.")
  .depgraph_assert(length(threshold) == 1L && is.numeric(threshold) && !is.na(threshold),
                   "`threshold` must be a single numeric value.")
  for (col in c(id1, id2, kinship)) {
    .depgraph_assert(col %in% names(pairs), paste0("Missing column in `pairs`: ", col))
  }

  value <- suppressWarnings(as.numeric(pairs[[kinship]]))
  keep <- !is.na(value) & value >= threshold &
    !is.na(pairs[[id1]]) & !is.na(pairs[[id2]]) &
    as.character(pairs[[id1]]) != as.character(pairs[[id2]])

  kept <- data.frame(
    from_id = as.character(pairs[[id1]])[keep],
    to_id = as.character(pairs[[id2]])[keep],
    kinship = value[keep],
    stringsAsFactors = FALSE
  )
  if (nrow(kept) == 0L) return(graph_edge_set())

  create_edges(
    kept,
    from_col = "from_id", to_col = "to_id",
    from_type = "Subject", to_type = "Subject",
    relation = "subject_related_to",
    attr_cols = "kinship"
  )
}

#' @rdname pairwise_edges
#' @export
spatial_edges_from_coords <- function(coords, radius, id = "sample_id", coord_cols = NULL) {
  .depgraph_assert(is.data.frame(coords), "`coords` must be a data.frame.")
  .depgraph_assert(length(radius) == 1L && is.numeric(radius) && !is.na(radius),
                   "`radius` must be a single numeric value.")
  .depgraph_assert(id %in% names(coords), paste0("Missing id column in `coords`: ", id))

  if (is.null(coord_cols)) {
    numeric_cols <- names(coords)[vapply(coords, is.numeric, logical(1))]
    coord_cols <- setdiff(numeric_cols, id)
  }
  .depgraph_assert(length(coord_cols) >= 1L,
                   "No coordinate columns found; supply `coord_cols`.")
  for (col in coord_cols) {
    .depgraph_assert(col %in% names(coords), paste0("Missing coordinate column in `coords`: ", col))
  }

  ids <- as.character(coords[[id]])
  mat <- as.matrix(coords[, coord_cols, drop = FALSE])
  storage.mode(mat) <- "double"

  empty <- data.frame(
    from_id = character(), to_id = character(), distance = numeric(),
    stringsAsFactors = FALSE
  )

  n <- nrow(mat)
  kept <- if (n < 2L) {
    empty
  } else {
    dmat <- as.matrix(stats::dist(mat))
    rows <- list()
    for (i in seq_len(n - 1L)) {
      for (j in seq(i + 1L, n)) {
        d <- dmat[i, j]
        if (!is.na(d) && d <= radius && ids[[i]] != ids[[j]]) {
          rows[[length(rows) + 1L]] <- data.frame(
            from_id = ids[[i]], to_id = ids[[j]], distance = d,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(rows) == 0L) empty else do.call(rbind, rows)
  }
  if (nrow(kept) == 0L) return(graph_edge_set())

  create_edges(
    kept,
    from_col = "from_id", to_col = "to_id",
    from_type = "Sample", to_type = "Sample",
    relation = "sample_adjacent_to",
    attr_cols = "distance"
  )
}

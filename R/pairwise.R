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

# Edges that connect in-scope samples for a pairwise mode, in a form
# `.depgraph_sample_components()` can consume without enumerating sample pairs:
# spatial   -> sample_adjacent_to edges among in-scope samples;
# relatedness -> sample -> subject edges for in-scope samples plus the
#                subject_related_to edges (samples of related subjects, and
#                samples of the same subject, then fall into one component).
.depgraph_pairwise_component_edges <- function(graph, mode, sample_node_ids) {
  relation <- .depgraph_pairwise_relation[[mode]]
  edge_data <- graph$edges$data

  if (identical(mode, "spatial")) {
    e <- edge_data[
      edge_data$edge_type == relation &
        edge_data$from %in% sample_node_ids &
        edge_data$to %in% sample_node_ids,
      c("from", "to"),
      drop = FALSE
    ]
    row.names(e) <- NULL
    return(e)
  }

  belongs <- edge_data[
    edge_data$edge_type == "sample_belongs_to_subject" & edge_data$from %in% sample_node_ids,
    c("from", "to"),
    drop = FALSE
  ]
  related <- edge_data[edge_data$edge_type == relation, c("from", "to"), drop = FALSE]
  # Keep only relatedness edges whose BOTH subjects carry in-scope samples. A
  # subject with no in-scope samples must not act as a bridge (P1 ~ P2 ~ P3 with
  # P2 sample-less does not group P1's and P3's samples), matching the pairwise
  # semantics the mode has always had and the subset rule used by composite mode.
  related <- related[related$from %in% belongs$to & related$to %in% belongs$to, , drop = FALSE]
  e <- rbind(belongs, related)
  row.names(e) <- NULL
  e
}

.derive_pairwise_constraints <- function(graph, mode, samples = NULL) {
  relation <- .depgraph_pairwise_relation[[mode]]
  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  keep_ids <- sample_nodes$node_id

  component_edges <- .depgraph_pairwise_component_edges(graph, mode, keep_ids)
  comps <- .depgraph_sample_components(keep_ids, component_edges)
  membership_idx <- comps$membership
  component_size <- comps$size

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
    missing_subject <- setdiff(
      keep_ids,
      graph$edges$data$from[graph$edges$data$edge_type == "sample_belongs_to_subject"]
    )
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
      n_dependency_edges = nrow(component_edges),
      threshold = as.numeric(graph$metadata$edge_sources[[relation]]$threshold %||% NA_real_),
      threshold_metric = as.character(graph$metadata$edge_sources[[relation]]$metric %||% NA_character_),
      warnings = warnings
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
#' @param pairs Either a data.frame of subject pairs with two id columns and a
#'   metric column (the long format written by KING, GCTA, and most kinship
#'   tools), or a square symmetric numeric matrix whose row names are subject
#'   ids (e.g. PLINK \code{--make-rel square} output); a matrix is expanded to
#'   its upper-triangle pairs before thresholding.
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
  if (is.matrix(pairs)) {
    pairs <- .depgraph_kinship_matrix_to_pairs(pairs, id1 = id1, id2 = id2, kinship = kinship)
  }
  .depgraph_assert(is.data.frame(pairs), "`pairs` must be a data.frame or a square kinship matrix.")
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
  out <- if (nrow(kept) == 0L) {
    graph_edge_set(source = list(relation = "subject_related_to", from_col = "from_id", to_col = "to_id"))
  } else {
    create_edges(
      kept,
      from_col = "from_id", to_col = "to_id",
      from_type = "Subject", to_type = "Subject",
      relation = "subject_related_to",
      attr_cols = "kinship"
    )
  }
  # Record the threshold on the edge set so the graph (and any derived
  # split_spec) can report the provenance of the grouping.
  out$source$threshold <- as.numeric(threshold)
  out$source$metric <- "kinship"
  out
}

# Convert a square, symmetric kinship / GRM matrix with subject ids as dimnames
# (e.g. PLINK `--make-rel square`) into the long pair table the edge builder
# consumes: one row per unordered pair from the upper triangle.
.depgraph_kinship_matrix_to_pairs <- function(mat, id1, id2, kinship) {
  .depgraph_assert(
    is.numeric(mat) && nrow(mat) == ncol(mat),
    "A kinship matrix must be square and numeric."
  )
  ids <- rownames(mat) %||% colnames(mat)
  .depgraph_assert(
    !is.null(ids) && length(ids) == nrow(mat) && all(nzchar(ids)),
    "A kinship matrix must carry subject ids as row (or column) names."
  )
  if (!is.null(colnames(mat)) && !identical(colnames(mat), ids)) {
    .depgraph_assert(
      setequal(colnames(mat), ids),
      "Row and column names of a kinship matrix must refer to the same subjects."
    )
    mat <- mat[, ids, drop = FALSE]
  }
  hit <- which(upper.tri(mat), arr.ind = TRUE)
  hit <- hit[order(hit[, 1L], hit[, 2L]), , drop = FALSE]
  out <- data.frame(
    ids[hit[, 1L]],
    ids[hit[, 2L]],
    mat[hit],
    stringsAsFactors = FALSE
  )
  names(out) <- c(id1, id2, kinship)
  out
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
    # Vectorised upper-triangle scan. `which()` drops NA distances; rows are
    # ordered (i, j) row-major to match the historical edge numbering.
    dmat <- as.matrix(stats::dist(mat))
    hit <- which(dmat <= radius & upper.tri(dmat), arr.ind = TRUE)
    if (nrow(hit) == 0L) {
      empty
    } else {
      hit <- hit[order(hit[, 1L], hit[, 2L]), , drop = FALSE]
      i <- hit[, 1L]
      j <- hit[, 2L]
      keep <- ids[i] != ids[j]
      data.frame(
        from_id = ids[i][keep],
        to_id = ids[j][keep],
        distance = dmat[hit][keep],
        stringsAsFactors = FALSE
      )
    }
  }
  out <- if (nrow(kept) == 0L) {
    graph_edge_set(source = list(relation = "sample_adjacent_to", from_col = "from_id", to_col = "to_id"))
  } else {
    create_edges(
      kept,
      from_col = "from_id", to_col = "to_id",
      from_type = "Sample", to_type = "Sample",
      relation = "sample_adjacent_to",
      attr_cols = "distance"
    )
  }
  out$source$threshold <- as.numeric(radius)
  out$source$metric <- "distance"
  out
}

# Feasibility comparison across constraint modes and bridge-node diagnostics.

#' Compare Split Constraint Modes and Diagnose Giant-Component Bridges
#'
#' \code{compare_split_constraints()} evaluates multiple candidate constraint
#' modes on the same \code{dependency_graph} and returns a side-by-side table of
#' group counts, largest-group concentration, effective group count (inverse
#' Herfindahl--Hirschman index), $k$-fold feasibility, severed vs. remaining
#' structural leakage warnings, and (when outcome strata are present) the
#' minimum number of distinct groups per outcome stratum. Validation diagnostics
#' whose severance is resolved (\code{TRUE} or \code{FALSE}) are counted in
#' \code{n_risks_severed} and \code{n_risks_remaining}, while indeterminate
#' (\code{NA}) diagnostics are excluded from both counts. When a constraint
#' collapses all samples into a single group (\code{n_groups < 2}), must-links
#' are preserved within that component (\code{severed = TRUE} in
#' \code{summarize_leakage_risks()}), but \code{compare_split_constraints()}
#' reports \code{n_risks_severed = 0L} and counts resolved risks in
#' \code{n_risks_remaining} because a single group cannot form any nontrivial
#' train/test split.
#'
#' \code{find_dependency_bridges()} identifies non-sample entity nodes (such as
#' shared batches, sites, or subjects) whose removal breaks connected components
#' in the sample-dependency bipartite graph, ranked by the number of components
#' gained and reduction in largest component size.
#'
#' @param graph A \code{dependency_graph}.
#' @param modes Character vector of constraint modes to compare. When omitted,
#'   all modes whose underlying dependency relation is present in \code{graph}
#'   are compared (plus \code{"composite"} when at least two relations exist).
#' @param k Target number of cross-validation folds used to evaluate
#'   \code{k_fold_feasible} (default \code{5L}).
#' @param samples Optional sample subset (sample IDs or node IDs).
#' @param composite_via Optional \code{via} vector passed to
#'   \code{derive_split_constraints()} when \code{"composite"} is evaluated.
#' @param via Dependency node types or pairwise mode names to include when
#'   searching for bridging entities in \code{find_dependency_bridges()}.
#' @param top_n Optional positive integer limiting the number of bridge rows
#'   returned.
#' @return \code{compare_split_constraints()} returns a
#'   \code{split_constraint_comparison} object (use \code{as.data.frame()} for
#'   the summary table). \code{find_dependency_bridges()} returns a
#'   \code{graph_query_result}.
#' @examples
#' meta <- data.frame(
#'   sample_id  = paste0("S", 1:8),
#'   subject_id = c("P1", "P1", "P2", "P2", "P3", "P3", "P4", "P4"),
#'   batch_id   = c("B1", "B1", "B1", "B2", "B2", "B2", "B3", "B3"),
#'   study_id   = rep(c("ST1", "ST2"), each = 4),
#'   outcome_id = rep(c("ctrl", "case"), 4)
#' )
#' g <- graph_from_metadata(meta)
#'
#' compare_split_constraints(g, k = 2)
#' find_dependency_bridges(g, via = c("Subject", "Batch"))
#' @export
compare_split_constraints <- function(graph,
                                      modes = NULL,
                                      k = 5L,
                                      samples = NULL,
                                      composite_via = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  .depgraph_assert(
    is.numeric(k) && length(k) == 1L && !is.na(k) && k >= 2,
    "`k` must be a single integer >= 2."
  )
  k <- as.integer(k)

  if (is.null(modes)) {
    present_edges <- unique(graph$edges$data$edge_type)
    all_single <- setdiff(.depgraph_constraint_modes, "composite")
    active <- vapply(all_single, function(m) {
      .depgraph_mode_relation(m) %in% present_edges
    }, logical(1))
    modes <- all_single[active]
    if (length(modes) == 0L) {
      modes <- "subject"
    } else if (length(modes) >= 2L) {
      modes <- c(modes, "composite")
    }
  } else {
    modes <- unique(vapply(modes, function(m) {
      .depgraph_normalize_constraint_mode(.depgraph_match_arg(m, .depgraph_constraint_modes, "modes"))
    }, character(1), USE.NAMES = FALSE))
  }

  val <- if (is.null(samples)) {
    validate_graph(graph)
  } else {
    validate_graph(subset_graph(graph, samples = samples, validate = FALSE))
  }
  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  stratum_vec <- stats::setNames(
    .split_spec_stratum_from_graph(graph, sample_nodes$node_id),
    sample_nodes$node_id
  )
  has_stratum <- any(!is.na(stratum_vec) & nzchar(as.character(stratum_vec)))

  rows <- lapply(modes, function(m) {
    via_arg <- if (identical(m, "composite")) {
      if (!is.null(composite_via)) {
        composite_via
      } else {
        setdiff(modes, "composite")
      }
    } else {
      NULL
    }
    if (identical(m, "composite") && length(via_arg) == 0L) {
      via_arg <- NULL
    }
    con <- derive_split_constraints(graph, mode = m, samples = samples, via = via_arg)
    smap <- con$sample_map
    n <- nrow(smap)
    g_sizes <- if (n > 0L) as.integer(table(smap$group_id)) else integer()
    n_groups <- length(g_sizes)
    max_g <- if (n_groups > 0L) max(g_sizes) else 0L
    max_share <- if (n > 0L) max_g / n else NA_real_
    shares <- if (n > 0L) g_sizes / n else numeric()
    eff_groups <- if (length(shares) > 0L && sum(shares^2) > 0) 1 / sum(shares^2) else NA_real_
    singleton_share <- if (n_groups > 0L) mean(g_sizes == 1L) else NA_real_

    # k-fold feasibility: at least k distinct groups and no single group
    # exceeding max(0.5, 2 / k) of the dataset.
    feasible <- isTRUE(n_groups >= k && !is.na(max_share) && max_share <= max(0.5, 2 / k))

    risk_df <- summarize_leakage_risks(graph, constraint = con, validation = val)$diagnostics
    val_risks <- risk_df[risk_df$source == "validation" & !is.na(risk_df$severed), , drop = FALSE]
    if (n_groups >= 2L) {
      n_severed <- sum(val_risks$severed, na.rm = TRUE)
      n_remaining <- sum(!val_risks$severed, na.rm = TRUE)
    } else {
      # A degenerate 1-group collapse cannot form any train/test split.
      n_severed <- 0L
      n_remaining <- nrow(val_risks)
    }

    min_stratum_groups <- NA_integer_
    if (has_stratum && n > 0L) {
      s_stratum <- unname(stratum_vec[smap$sample_node_id])
      valid_s <- !is.na(s_stratum) & nzchar(as.character(s_stratum))
      if (any(valid_s)) {
        groups_by_stratum <- split(smap$group_id[valid_s], s_stratum[valid_s])
        min_stratum_groups <- as.integer(min(vapply(groups_by_stratum, function(g) length(unique(g)), integer(1))))
      }
    }

    data.frame(
      mode = m,
      strategy = as.character(con$strategy),
      n_samples = as.integer(n),
      n_groups = as.integer(n_groups),
      max_group_size = as.integer(max_g),
      max_group_share = as.numeric(max_share),
      effective_n_groups = as.numeric(eff_groups),
      singleton_share = as.numeric(singleton_share),
      k_fold_feasible = feasible,
      n_risks_severed = as.integer(n_severed),
      n_risks_remaining = as.integer(n_remaining),
      min_stratum_groups = min_stratum_groups,
      stringsAsFactors = FALSE
    )
  })

  table <- if (length(rows) == 0L) {
    data.frame(
      mode = character(),
      strategy = character(),
      n_samples = integer(),
      n_groups = integer(),
      max_group_size = integer(),
      max_group_share = numeric(),
      effective_n_groups = numeric(),
      singleton_share = numeric(),
      k_fold_feasible = logical(),
      n_risks_severed = integer(),
      n_risks_remaining = integer(),
      min_stratum_groups = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    row.names(out) <- NULL
    out
  }

  structure(
    list(
      table = table,
      k = k,
      metadata = list(
        graph_name = graph$metadata$graph_name,
        dataset_name = graph$metadata$dataset_name,
        n_samples = nrow(sample_nodes)
      )
    ),
    class = "split_constraint_comparison"
  )
}

#' @rdname compare_split_constraints
#' @export
find_dependency_bridges <- function(graph,
                                    via = c("Subject", "Batch", "Study", "Site",
                                            "Region", "Platform", "Assay"),
                                    samples = NULL,
                                    top_n = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  if (!is.null(top_n)) {
    .depgraph_assert(
      is.numeric(top_n) && length(top_n) == 1L && !is.na(top_n) && top_n >= 1,
      "`top_n` must be a single positive integer."
    )
    top_n <- as.integer(top_n)
  }

  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  keep_ids <- sample_nodes$node_id
  via_parsed <- .depgraph_parse_via_sources(via)
  dep_edges <- .depgraph_dependency_edges(graph, keep_ids, via_parsed$edge_types)[, c("from", "to"), drop = FALSE]
  for (pm in via_parsed$pairwise_modes) {
    dep_edges <- rbind(dep_edges, .depgraph_pairwise_component_edges(graph, pm, keep_ids))
  }

  base_comps <- .depgraph_sample_components(keep_ids, dep_edges)
  base_n_comp <- length(unique(base_comps$membership))
  base_max_size <- if (length(base_comps$size) > 0L) max(base_comps$size) else 0L

  node_data <- graph$nodes$data
  target_ids <- setdiff(unique(c(dep_edges$from, dep_edges$to)), keep_ids)
  # Only evaluate non-sample target nodes linked to at least 2 samples/edges.
  deg_tab <- table(c(dep_edges$from[!dep_edges$from %in% keep_ids], dep_edges$to[!dep_edges$to %in% keep_ids]))
  candidate_targets <- names(deg_tab)[deg_tab >= 2L]
  candidate_targets <- intersect(candidate_targets, target_ids)

  if ("spatial" %in% via_parsed$pairwise_modes) {
    sp_edges <- .depgraph_pairwise_component_edges(graph, "spatial", keep_ids)
    if (nrow(sp_edges) > 0L) {
      sp_deg <- table(c(sp_edges$from, sp_edges$to))
      candidate_targets <- unique(c(candidate_targets, names(sp_deg)[sp_deg >= 2L]))
    }
  }

  rows <- lapply(candidate_targets, function(tid) {
    sub_edges <- dep_edges[dep_edges$from != tid & dep_edges$to != tid, , drop = FALSE]
    rem_samples <- if (tid %in% keep_ids) setdiff(keep_ids, tid) else keep_ids
    sub_comps <- .depgraph_sample_components(rem_samples, sub_edges)
    without_n_comp <- length(unique(sub_comps$membership))
    without_max_size <- if (length(sub_comps$size) > 0L) max(sub_comps$size) else 0L
    n_linked <- sum((dep_edges$from %in% keep_ids & dep_edges$to == tid) |
                      (dep_edges$to %in% keep_ids & dep_edges$from == tid))
    nrow_idx <- match(tid, node_data$node_id)
    data.frame(
      node_id = tid,
      node_type = as.character(node_data$node_type[nrow_idx]),
      node_key = as.character(node_data$node_key[nrow_idx]),
      n_samples_linked = as.integer(n_linked),
      n_components_before = as.integer(base_n_comp),
      n_components_without = as.integer(without_n_comp),
      components_gained = as.integer(without_n_comp - base_n_comp),
      max_component_size_before = as.integer(base_max_size),
      max_component_size_without = as.integer(without_max_size),
      max_size_reduction = as.integer(base_max_size - without_max_size),
      is_bridge = isTRUE(without_n_comp > base_n_comp),
      stringsAsFactors = FALSE
    )
  })

  table <- if (length(rows) == 0L) {
    data.frame(
      node_id = character(),
      node_type = character(),
      node_key = character(),
      n_samples_linked = integer(),
      n_components_before = integer(),
      n_components_without = integer(),
      components_gained = integer(),
      max_component_size_before = integer(),
      max_component_size_without = integer(),
      max_size_reduction = integer(),
      is_bridge = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    ord <- order(-out$components_gained, -out$max_size_reduction, -out$n_samples_linked, out$node_id)
    out <- out[ord, , drop = FALSE]
    if (!is.null(top_n) && nrow(out) > top_n) {
      out <- out[seq_len(top_n), , drop = FALSE]
    }
    row.names(out) <- NULL
    out
  }

  active_edge_types <- c(via_parsed$edge_types, unname(.depgraph_pairwise_relation[via_parsed$pairwise_modes]))
  bridge_edge_ids <- if (nrow(table) > 0L) {
    ed <- graph$edges$data
    ed$edge_id[
      ed$edge_type %in% active_edge_types &
        (ed$from %in% table$node_id | ed$to %in% table$node_id)
    ]
  } else {
    character()
  }

  .depgraph_query_result(
    graph = graph,
    query = "find_dependency_bridges",
    params = list(via = via, samples = samples, top_n = top_n),
    node_ids = table$node_id,
    edge_ids = bridge_edge_ids,
    table = table,
    metadata = list(
      n_components_before = base_n_comp,
      max_component_size_before = base_max_size,
      n_bridges = sum(table$is_bridge)
    )
  )
}

#' @export
print.split_constraint_comparison <- function(x, ...) {
  cat("<split_constraint_comparison> (k =", x$k, "folds)\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
summary.split_constraint_comparison <- function(object, ...) {
  list(
    k = object$k,
    n_modes = nrow(object$table),
    feasible_modes = object$table$mode[object$table$k_fold_feasible],
    metadata = object$metadata
  )
}

#' @export
as.data.frame.split_constraint_comparison <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$table
}

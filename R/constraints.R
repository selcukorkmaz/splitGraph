# Split-constraint derivation for dependency_graph objects.

.depgraph_constraint_mode_map <- c(
  subject = "Subject",
  batch = "Batch",
  study = "Study",
  time = "Timepoint",
  site = "Site",
  region = "Region",
  platform = "Platform",
  assay = "Assay"
)

.depgraph_constraint_edge_map <- c(
  subject = "sample_belongs_to_subject",
  batch = "sample_processed_in_batch",
  study = "sample_from_study",
  time = "sample_collected_at_timepoint",
  site = "sample_collected_at_site",
  region = "sample_located_in_region",
  platform = "sample_run_on_platform",
  assay = "sample_measured_by_assay"
)

# Every mode `derive_split_constraints()` accepts, in the order they appear in
# its `mode` default. Single source of truth for the formal default, the
# argument check, and the classed-error choices.
.depgraph_constraint_modes <- c(
  "subject", "batch", "study", "time", "site", "region",
  "platform", "assay", "relatedness", "spatial", "composite"
)

.depgraph_normalize_constraint_mode <- function(mode) {
  mode <- tolower(as.character(mode)[1L])
  .depgraph_assert(
    mode %in% .depgraph_constraint_modes,
    paste0("Unsupported constraint mode: ", mode)
  )
  mode
}

# Modes that may be combined inside a composite derivation: every direct
# (single-target) mode plus the pairwise modes.
.depgraph_composable_modes <- function() {
  c(names(.depgraph_constraint_mode_map), names(.depgraph_pairwise_relation))
}

.depgraph_is_pairwise_mode <- function(mode) {
  mode %in% names(.depgraph_pairwise_relation)
}

.depgraph_normalize_constraint_modes <- function(modes) {
  modes <- unique(tolower(as.character(modes)))
  .depgraph_assert(length(modes) > 0L, "At least one constraint mode is required.")
  .depgraph_assert(all(modes %in% .depgraph_composable_modes()), paste0(
    "Unsupported constraint mode(s): ",
    paste(setdiff(modes, .depgraph_composable_modes()), collapse = ", ")
  ))
  modes
}

# Relation (edge type) that a composable mode contributes.
.depgraph_mode_relation <- function(mode) {
  if (.depgraph_is_pairwise_mode(mode)) .depgraph_pairwise_relation[[mode]] else .depgraph_constraint_edge_map[[mode]]
}

.depgraph_constraint_samples <- function(graph, samples = NULL) {
  sample_ids <- .depgraph_resolve_sample_node_ids(graph, samples)
  sample_nodes <- graph$nodes$data[graph$nodes$data$node_type == "Sample", , drop = FALSE]
  sample_nodes[match(sample_ids, sample_nodes$node_id), , drop = FALSE]
}

.depgraph_direct_assignment <- function(graph, mode, samples = NULL) {
  mode <- .depgraph_normalize_constraint_mode(mode)
  .depgraph_assert(mode != "composite", "Composite assignments must be derived separately.")

  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  edge_type <- .depgraph_constraint_edge_map[[mode]]
  target_type <- .depgraph_constraint_mode_map[[mode]]

  edges <- edge_data[
    edge_data$edge_type == edge_type &
      edge_data$from %in% sample_nodes$node_id,
    c("from", "to", "edge_type"),
    drop = FALSE
  ]

  duplicate_sources <- unique(edges$from[duplicated(edges$from)])
  if (length(duplicate_sources) > 0L) {
    duplicate_samples <- sample_nodes$node_key[match(duplicate_sources, sample_nodes$node_id)]
    allow_multi <- identical(mode, "subject") &&
      .depgraph_validation_override(graph, "allow_multi_subject_samples", default = FALSE)
    if (!allow_multi) {
      hint <- if (identical(mode, "subject")) {
        " (set `validation_overrides = list(allow_multi_subject_samples = TRUE)` to allow this and pick the first listed assignment.)"
      } else {
        ""
      }
      .depgraph_stop(
        paste0(
          "Multiple ", mode, " assignments found for sample(s): ",
          paste(duplicate_samples, collapse = ", "),
          hint
        ),
        class = "splitgraph_ambiguity_error",
        code = paste0("sample_multiple_", mode, "_assignments")
      )
    }
    multi_assignment_samples <- duplicate_samples
  } else {
    multi_assignment_samples <- character()
  }

  edges <- edges[!duplicated(edges$from), , drop = FALSE]

  # Vectorised lookup: one edge per sample at most, then one node row per edge.
  edge_idx <- match(sample_nodes$node_id, edges$from)
  target_idx <- match(edges$to[edge_idx], node_data$node_id)

  out <- data.frame(
    sample_id = sample_nodes$node_key,
    sample_node_id = sample_nodes$node_id,
    linked_node_id = node_data$node_id[target_idx],
    linked_node_type = target_type,
    linked_key = node_data$node_key[target_idx],
    linked_label = node_data$label[target_idx],
    edge_type = edge_type,
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  attr(out, "multi_assignment_samples") <- multi_assignment_samples
  out
}

.depgraph_warning_if_missing <- function(assignments, mode) {
  missing_samples <- assignments$sample_id[is.na(assignments$linked_node_id)]
  if (length(missing_samples) == 0L) {
    return(character())
  }

  paste0(
    "Samples missing ", mode, " assignments were retained as singleton groups: ",
    paste(missing_samples, collapse = ", ")
  )
}

.depgraph_build_sample_map <- function(assignments, mode, explanation_prefix = NULL) {
  mode <- .depgraph_normalize_constraint_mode(mode)
  prefix <- explanation_prefix %||% paste0("Grouped by ", mode)

  linked <- !is.na(assignments$linked_node_id) & nzchar(assignments$linked_node_id)
  linked[is.na(linked)] <- FALSE

  out <- data.frame(
    sample_id = assignments$sample_id,
    sample_node_id = assignments$sample_node_id,
    group_id = ifelse(
      linked,
      paste0(mode, ":", assignments$linked_key),
      paste0(mode, ":unlinked:", assignments$sample_id)
    ),
    constraint_type = mode,
    group_label = ifelse(linked, assignments$linked_key, paste0("unlinked_", assignments$sample_id)),
    explanation = ifelse(
      linked,
      paste0(prefix, " through ", assignments$edge_type, " -> ", assignments$linked_key, "."),
      paste0("No ", mode, " assignment was available; sample retained as an unlinked singleton group.")
    ),
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  out
}

.depgraph_compute_time_order <- function(graph, time_node_ids) {
  time_nodes <- graph$nodes$data[match(time_node_ids, graph$nodes$data$node_id), , drop = FALSE]
  raw_time_index <- vapply(
    time_nodes$attrs,
    function(x) {
      value <- .depgraph_extract_attr(x, "time_index")
      if (length(value) == 0L || is.na(value)) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(as.character(value)))
    },
    FUN.VALUE = numeric(1)
  )

  warnings <- character()
  order_rank <- rep(NA_integer_, length(time_node_ids))
  names(order_rank) <- time_node_ids
  names(raw_time_index) <- time_node_ids

  if (length(time_node_ids) == 0L) {
    return(list(
      source = "none",
      time_index = raw_time_index,
      order_rank = order_rank,
      warnings = warnings
    ))
  }

  time_consistency <- .depgraph_time_order_consistency(graph$nodes$data, graph$edges$data)
  if (length(time_consistency$self_loop_edge_ids) > 0L || length(time_consistency$cycle_edge_ids) > 0L) {
    .depgraph_stop(
      "Time ordering metadata are inconsistent: `timepoint_precedes` must define an acyclic ordering.",
      class = "splitgraph_validation_error",
      code = "timepoint_precedence_cycle"
    )
  }
  if (nrow(time_consistency$conflicting_pairs) > 0L) {
    pair_labels <- paste0(
      time_consistency$conflicting_pairs$from,
      " -> ",
      time_consistency$conflicting_pairs$to
    )
    .depgraph_stop(
      paste0(
        "Time ordering metadata conflict between `time_index` and `timepoint_precedes`: ",
        paste(pair_labels, collapse = ", ")
      ),
      class = "splitgraph_validation_error",
      code = "time_order_conflict"
    )
  }

  if (all(!is.na(raw_time_index))) {
    unique_order <- sort(unique(raw_time_index))
    rank_map <- stats::setNames(seq_along(unique_order), as.character(unique_order))
    order_rank <- as.integer(rank_map[as.character(raw_time_index)])
    names(order_rank) <- time_node_ids
    return(list(
      source = "time_index",
      time_index = raw_time_index,
      order_rank = order_rank,
      warnings = warnings
    ))
  }

  precedence_edges <- graph$edges$data[
    graph$edges$data$edge_type == "timepoint_precedes" &
      graph$edges$data$from %in% time_node_ids &
      graph$edges$data$to %in% time_node_ids,
    c("from", "to"),
    drop = FALSE
  ]

  if (nrow(precedence_edges) > 0L) {
    precedence_graph <- igraph::graph_from_data_frame(
      d = precedence_edges,
      vertices = data.frame(name = time_node_ids, stringsAsFactors = FALSE),
      directed = TRUE
    )
    if (igraph::is_dag(precedence_graph)) {
      topo <- igraph::as_ids(igraph::topo_sort(precedence_graph, mode = "out"))
      order_rank[topo] <- seq_along(topo)
      return(list(
        source = "timepoint_precedes",
        time_index = raw_time_index,
        order_rank = order_rank,
        warnings = warnings
      ))
    }

    warnings <- c(warnings, "Timepoint precedence edges are cyclic; ordering metadata could not be derived from graph structure.")
  }

  if (any(!is.na(raw_time_index))) {
    available <- sort(unique(raw_time_index[!is.na(raw_time_index)]))
    rank_map <- stats::setNames(seq_along(available), as.character(available))
    order_rank[!is.na(raw_time_index)] <- as.integer(rank_map[as.character(raw_time_index[!is.na(raw_time_index)])])
    warnings <- c(warnings, "Time ordering is only partially available because some timepoints are missing `time_index` values.")
  } else {
    warnings <- c(warnings, "Time ordering is unavailable because neither `time_index` metadata nor usable `timepoint_precedes` edges were found.")
  }

  list(
    source = "partial",
    time_index = raw_time_index,
    order_rank = order_rank,
    warnings = warnings
  )
}

.derive_subject_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "subject", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "subject")
  multi_subject_samples <- attr(assignments, "multi_assignment_samples", exact = TRUE)
  if (!is.null(multi_subject_samples) && length(multi_subject_samples) > 0L) {
    warnings <- c(
      warnings,
      paste0(
        "Samples linked to multiple subjects were assigned to their first listed subject ",
        "(allow_multi_subject_samples override): ",
        paste(multi_subject_samples, collapse = ", ")
      )
    )
  }
  sample_map <- .depgraph_build_sample_map(assignments, "subject")

  split_constraint(
    strategy = "subject",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = NULL,
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "subject",
      strategy = "subject",
      relations_used = "sample_belongs_to_subject",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_batch_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "batch", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "batch")
  sample_map <- .depgraph_build_sample_map(assignments, "batch")

  split_constraint(
    strategy = "batch",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "batch",
      strategy = "batch",
      relations_used = "sample_processed_in_batch",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_study_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "study", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "study")
  sample_map <- .depgraph_build_sample_map(assignments, "study")

  split_constraint(
    strategy = "study",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "study",
      strategy = "study",
      relations_used = "sample_from_study",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_site_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "site", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "site")
  sample_map <- .depgraph_build_sample_map(assignments, "site")

  split_constraint(
    strategy = "site",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "site",
      strategy = "site",
      relations_used = "sample_collected_at_site",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_region_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "region", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "region")
  sample_map <- .depgraph_build_sample_map(assignments, "region")

  split_constraint(
    strategy = "region",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "region",
      strategy = "region",
      relations_used = "sample_located_in_region",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_platform_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "platform", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "platform")
  sample_map <- .depgraph_build_sample_map(assignments, "platform")

  split_constraint(
    strategy = "platform",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "platform",
      strategy = "platform",
      relations_used = "sample_run_on_platform",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_assay_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "assay", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "assay")
  sample_map <- .depgraph_build_sample_map(assignments, "assay")

  split_constraint(
    strategy = "assay",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = "group_id",
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "assay",
      strategy = "assay",
      relations_used = "sample_measured_by_assay",
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

.derive_time_constraints <- function(graph, samples = NULL) {
  assignments <- .depgraph_direct_assignment(graph, "time", samples = samples)
  warnings <- .depgraph_warning_if_missing(assignments, "timepoint")
  valid_time_nodes <- stats::na.omit(assignments$linked_node_id)
  order_info <- .depgraph_compute_time_order(graph, unique(valid_time_nodes))
  warnings <- unique(c(warnings, order_info$warnings))

  order_rank <- order_info$order_rank[assignments$linked_node_id]
  order_rank[is.na(assignments$linked_node_id)] <- NA_integer_
  time_index <- order_info$time_index[assignments$linked_node_id]
  time_index[is.na(assignments$linked_node_id)] <- NA_real_
  timepoint_id <- assignments$linked_key

  sample_map <- .depgraph_build_sample_map(assignments, "time")
  sample_map$time_index <- unname(time_index)
  sample_map$timepoint_id <- timepoint_id
  sample_map$order_rank <- unname(as.integer(order_rank))
  has_complete_ordering <- nrow(sample_map) > 0L && all(!is.na(sample_map$order_rank))

  split_constraint(
    strategy = "time",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = NULL,
      time_var = if (any(!is.na(sample_map$order_rank))) "order_rank" else NULL,
      ordering_required = has_complete_ordering
    ),
    metadata = list(
      mode = "time",
      strategy = "time",
      relations_used = c("sample_collected_at_timepoint", if (identical(order_info$source, "timepoint_precedes")) "timepoint_precedes" else NULL),
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      time_order_source = order_info$source,
      warnings = warnings
    )
  )
}

.depgraph_normalize_via_modes <- function(via) {
  if (is.null(via)) {
    return(c("subject", "batch", "study", "time"))
  }

  via <- as.character(via)
  via_modes <- vapply(via, function(x) {
    x_lower <- tolower(x)
    # A mode name: direct ("subject", ...) or pairwise ("relatedness", "spatial").
    if (x_lower %in% .depgraph_composable_modes()) {
      return(x_lower)
    }
    # A pairwise relation name is accepted as an alias for its mode.
    rel_idx <- match(x_lower, tolower(.depgraph_pairwise_relation))
    if (!is.na(rel_idx)) {
      return(names(.depgraph_pairwise_relation)[[rel_idx]])
    }
    # A node type ("Subject", ...) maps to its direct mode.
    match_idx <- match(x_lower, tolower(.depgraph_constraint_mode_map))
    .depgraph_assert(!is.na(match_idx), paste0("Unsupported composite dependency source: ", x))
    names(.depgraph_constraint_mode_map)[[match_idx]]
  }, character(1), USE.NAMES = FALSE)

  unique(via_modes)
}

# Human-readable labels for `metadata$via`: node types for direct modes,
# mode names for pairwise modes (which have no target node type).
.depgraph_via_labels <- function(via_modes) {
  vapply(via_modes, function(mode) {
    if (.depgraph_is_pairwise_mode(mode)) mode else unname(.depgraph_constraint_mode_map[[mode]])
  }, character(1), USE.NAMES = FALSE)
}

.derive_composite_strict_constraints <- function(graph, samples = NULL, via = NULL) {
  via_modes <- .depgraph_normalize_via_modes(via)
  direct_modes <- via_modes[!.depgraph_is_pairwise_mode(via_modes)]
  pairwise_modes <- via_modes[.depgraph_is_pairwise_mode(via_modes)]
  via_types <- .depgraph_via_labels(via_modes)
  edge_types <- vapply(via_modes, .depgraph_mode_relation, character(1), USE.NAMES = FALSE)

  # Components are computed on the bipartite sample-target graph restricted to
  # the requested samples. Restricting the *edges* to in-subset samples is what
  # keeps two in-subset samples apart when their only link runs through an
  # out-of-subset sample: that sample's edges are simply absent, so the two
  # targets it would have bridged stay disconnected. Pairwise relations add
  # their own connecting edges (sample-sample for spatial, sample-subject plus
  # subject-subject for relatedness) to the same component search.
  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  keep_ids <- sample_nodes$node_id
  dep_edges <- .depgraph_dependency_edges(graph, keep_ids, unname(.depgraph_constraint_edge_map[direct_modes]))[, c("from", "to"), drop = FALSE]
  for (mode in pairwise_modes) {
    dep_edges <- rbind(dep_edges, .depgraph_pairwise_component_edges(graph, mode, keep_ids))
  }
  comps <- .depgraph_sample_components(keep_ids, dep_edges)

  component_id <- paste0("component_", comps$membership)
  sample_map <- data.frame(
    sample_id = sample_nodes$node_key,
    sample_node_id = keep_ids,
    group_id = component_id,
    constraint_type = "composite_strict",
    group_label = component_id,
    explanation = paste0(
      "Strict composite grouping via transitive closure over: ",
      paste(via_types, collapse = ", "),
      "."
    ),
    stringsAsFactors = FALSE
  )
  row.names(sample_map) <- NULL

  warnings <- character()
  if (nrow(sample_map) > 0L && mean(comps$size == 1L) > 0.5) {
    warnings <- c(warnings, "Most strict composite groups are singletons; dependency coverage may be sparse.")
  }

  split_constraint(
    strategy = "strict",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = NULL,
      time_var = NULL,
      ordering_required = FALSE
    ),
    metadata = list(
      mode = "composite",
      strategy = "strict",
      relations_used = edge_types,
      via = via_types,
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      n_dependency_edges = nrow(dep_edges),
      warnings = warnings
    )
  )
}

.derive_rule_based_constraints <- function(graph, samples = NULL, priority = NULL, via = NULL) {
  via_modes <- .depgraph_normalize_via_modes(via)
  priority <- if (is.null(priority)) via_modes else .depgraph_normalize_constraint_modes(priority)
  .depgraph_assert(all(priority %in% via_modes), "`priority` must be a subset of the selected composite dependency sources.")

  sample_nodes <- .depgraph_constraint_samples(graph, samples)
  sample_ids <- sample_nodes$node_id
  n <- length(sample_ids)

  # One aligned key vector per prioritized mode (NA where the sample has no
  # assignment). Direct modes use the single linked target; pairwise modes use
  # the connected-component label, and a singleton component counts as "no
  # assignment" so the sample falls through to the next mode in priority.
  keys <- lapply(priority, function(mode) {
    if (.depgraph_is_pairwise_mode(mode)) {
      pairwise <- .derive_pairwise_constraints(graph, mode, samples = sample_ids)$sample_map
      idx <- match(sample_ids, pairwise$sample_node_id)
      group_size <- table(pairwise$group_id)[pairwise$group_id[idx]]
      key <- pairwise$group_label[idx]
      key[is.na(group_size) | group_size <= 1L] <- NA_character_
      return(key)
    }
    assignment <- .depgraph_direct_assignment(graph, mode, samples = sample_ids)
    assignment$linked_key[match(sample_ids, assignment$sample_node_id)]
  })
  names(keys) <- priority
  key_matrix <- matrix(unlist(keys, use.names = FALSE), nrow = n, ncol = length(priority))
  available <- !is.na(key_matrix)

  # Highest-priority available mode per sample (column index of first TRUE).
  first_idx <- apply(available, 1L, function(row) {
    hit <- which(row)
    if (length(hit) == 0L) NA_integer_ else hit[[1L]]
  })
  if (n == 0L) first_idx <- integer()
  has_mode <- !is.na(first_idx)
  chosen_mode <- ifelse(has_mode, priority[first_idx], NA_character_)
  chosen_key <- ifelse(has_mode, key_matrix[cbind(seq_len(n), ifelse(has_mode, first_idx, 1L))], NA_character_)

  # "Additional available dependencies" detail: every other available mode.
  additional <- vapply(seq_len(n), function(i) {
    if (!has_mode[[i]]) return("")
    others <- which(available[i, ] & seq_along(priority) != first_idx[[i]])
    if (length(others) == 0L) return("")
    paste0(
      " Additional available dependencies: ",
      paste(paste0(priority[others], "=", key_matrix[i, others]), collapse = ", "),
      "."
    )
  }, character(1))

  time_info <- if ("time" %in% priority) .derive_time_constraints(graph, samples = sample_ids)$sample_map else NULL
  time_idx <- if (is.null(time_info)) rep(NA_integer_, n) else match(sample_ids, time_info$sample_node_id)

  sample_map <- data.frame(
    sample_id = sample_nodes$node_key,
    sample_node_id = sample_ids,
    group_id = ifelse(
      has_mode,
      paste0("composite_", chosen_mode, ":", chosen_key),
      paste0("composite:unlinked:", sample_nodes$node_key)
    ),
    constraint_type = ifelse(has_mode, chosen_mode, "unlinked"),
    group_label = ifelse(has_mode, chosen_key, paste0("unlinked_", sample_nodes$node_key)),
    explanation = ifelse(
      has_mode,
      paste0(
        "Composite rule-based grouping selected ", chosen_mode,
        " based on priority order ", paste(priority, collapse = " > "),
        " -> ", chosen_key, ".", additional
      ),
      "No prioritized dependency assignment was available; sample retained as a singleton group."
    ),
    time_index = if (is.null(time_info)) rep(NA_real_, n) else as.numeric(time_info$time_index[time_idx]),
    timepoint_id = if (is.null(time_info)) rep(NA_character_, n) else as.character(time_info$timepoint_id[time_idx]),
    order_rank = if (is.null(time_info)) rep(NA_integer_, n) else as.integer(time_info$order_rank[time_idx]),
    stringsAsFactors = FALSE
  )
  row.names(sample_map) <- NULL
  warnings <- character()
  if (any(sample_map$constraint_type == "unlinked")) {
    warnings <- c(warnings, paste0(
      "Some samples did not match any prioritized dependency and were retained as singleton groups: ",
      paste(sample_map$sample_id[sample_map$constraint_type == "unlinked"], collapse = ", ")
    ))
  }

  split_constraint(
    strategy = "rule_based",
    sample_map = sample_map,
    recommended_downstream_args = list(
      group_var = "group_id",
      block_var = NULL,
      time_var = if (any(!is.na(sample_map$order_rank))) "order_rank" else NULL,
      ordering_required = nrow(sample_map) > 0L && all(!is.na(sample_map$order_rank))
    ),
    metadata = list(
      mode = "composite",
      strategy = "rule_based",
      relations_used = vapply(priority, .depgraph_mode_relation, character(1), USE.NAMES = FALSE),
      via = .depgraph_via_labels(via_modes),
      priority = priority,
      n_groups = length(unique(sample_map$group_id)),
      n_samples = nrow(sample_map),
      warnings = warnings
    )
  )
}

#' Derive Split Constraints from Dependency Graphs
#'
#' Convert dataset dependency structure into deterministic sample-level
#' grouping constraints suitable for leakage-aware evaluation design.
#'
#' Constraint derivation rules:
#'
#' \describe{
#'   \item{\code{mode = "subject"}}{Groups samples by the target of
#'   \code{sample_belongs_to_subject}. All samples linked to the same
#'   \code{Subject} receive the same \code{group_id}.}
#'
#'   \item{\code{mode = "batch"}}{Groups samples by the target of
#'   \code{sample_processed_in_batch}. Samples with no batch assignment are
#'   retained as singleton unlinked groups and recorded in metadata warnings.}
#'
#'   \item{\code{mode = "study"}}{Groups samples by the target of
#'   \code{sample_from_study}.}
#'
#'   \item{\code{mode = "site"}}{Groups samples by the target of
#'   \code{sample_collected_at_site}. Samples with no site assignment are
#'   retained as singleton unlinked groups and recorded in metadata warnings.}
#'
#'   \item{\code{mode = "region"}}{Groups samples by the target of
#'   \code{sample_located_in_region} (e.g. a categorical tissue or anatomical
#'   region). Samples with no region assignment are retained as singleton
#'   unlinked groups and recorded in metadata warnings.}
#'
#'   \item{\code{mode = "platform"}}{Groups samples by the target of
#'   \code{sample_run_on_platform} (the sequencing / measurement platform or
#'   instrument). Samples with no platform assignment are retained as singleton
#'   unlinked groups and recorded in metadata warnings.}
#'
#'   \item{\code{mode = "assay"}}{Groups samples by the target of
#'   \code{sample_measured_by_assay} (the assay / modality). Samples with no
#'   assay assignment are retained as singleton unlinked groups and recorded in
#'   metadata warnings.}
#'
#'   \item{\code{mode = "relatedness"}}{Groups samples by transitive closure
#'   over thresholded \code{subject_related_to} edges (genetic relatedness).
#'   Samples that share a subject, or whose subjects are directly or indirectly
#'   related above threshold, land in the same connected-component group. Build
#'   the edges with \code{\link{relatedness_edges_from_kinship}}. Samples with
#'   no subject are retained as singleton groups (recorded in metadata
#'   warnings).}
#'
#'   \item{\code{mode = "spatial"}}{Groups samples by transitive closure over
#'   thresholded \code{sample_adjacent_to} edges (spatial proximity). Build the
#'   edges with \code{\link{spatial_edges_from_coords}}. Isolated samples form
#'   singleton groups.}
#'
#'   \item{\code{mode = "time"}}{Groups samples by the target of
#'   \code{sample_collected_at_timepoint}. When \code{Timepoint} nodes have
#'   \code{time_index} metadata, that value is used to derive
#'   \code{order_rank}. If \code{time_index} is unavailable, the function
#'   attempts to derive ordering from \code{timepoint_precedes} edges over the
#'   timepoint subgraph.}
#'
#'   \item{\code{mode = "composite"}, \code{strategy = "strict"}}{Projects the
#'   selected dependency relations onto a sample graph and assigns one
#'   \code{group_id} per connected component. This is the transitive-closure
#'   interpretation of composite dependency grouping.}
#'
#'   \item{\code{mode = "composite"}, \code{strategy = "rule_based"}}{Evaluates
#'   dependency assignments in deterministic priority order and groups each
#'   sample by the highest-priority available dependency source.
#'   Lower-priority available dependencies are retained in the explanation
#'   field.}
#' }
#'
#' The returned \code{split_constraint$sample_map} always contains
#' \code{sample_id}, \code{sample_node_id}, \code{group_id},
#' \code{constraint_type}, \code{group_label}, and \code{explanation}.
#' Time-aware constraints also include \code{time_index}, \code{timepoint_id},
#' and \code{order_rank} when available.
#'
#' Ambiguous direct assignments are rejected. A sample cannot be assigned to
#' multiple batches, studies, or timepoints when deriving direct split
#' constraints.
#'
#' @param graph A \code{dependency_graph}.
#' @param mode Constraint derivation mode.
#' @param samples Optional sample identifiers or sample node IDs used to
#'   restrict the returned \code{sample_map}. All requested samples must
#'   resolve successfully.
#' @param strategy Composite grouping strategy. Ignored for non-composite
#'   modes.
#' @param via Optional dependency sources used for composite grouping. May be
#'   given as lower-case modes such as \code{"subject"} or node types such as
#'   \code{"Subject"}. Any direct-assignment source (\code{"subject"},
#'   \code{"batch"}, \code{"study"}, \code{"time"}, \code{"site"},
#'   \code{"region"}, \code{"platform"}, \code{"assay"}) and either pairwise
#'   source (\code{"relatedness"}, \code{"spatial"}) can be combined. In the
#'   strict strategy a pairwise source contributes its thresholded edges to the
#'   same connected-component search as the direct relations; in the
#'   rule-based strategy it contributes the component label, and a singleton
#'   component counts as "no assignment" so the sample falls through to the
#'   next mode. Defaults to \code{c("subject", "batch", "study", "time")}.
#' @param priority Optional priority order used for
#'   \code{strategy = "rule_based"}.
#' @param include_warnings Whether to retain human-readable warnings in the
#'   returned metadata.
#' @param x A \code{split_constraint}.
#' @return \code{derive_split_constraints()} returns a \code{split_constraint}
#'   whose \code{sample_map} contains grouping assignments and, for time-aware
#'   constraints, ordering metadata. \code{grouping_vector()} returns a named
#'   character vector of \code{group_id} values keyed by \code{sample_id}.
#' @examples
#' meta <- data.frame(
#'   sample_id  = c("S1", "S2", "S3", "S4"),
#'   subject_id = c("P1", "P1", "P2", "P2"),
#'   batch_id   = c("B1", "B2", "B1", "B2")
#' )
#' g <- graph_from_metadata(meta)
#'
#' constraint <- derive_split_constraints(g, mode = "subject")
#' grouping_vector(constraint)
#' @export
derive_split_constraints <- function(graph,
                                     mode = c("subject", "batch", "study", "time", "site", "region",
                                              "platform", "assay", "relatedness", "spatial", "composite"),
                                     samples = NULL,
                                     strategy = c("strict", "rule_based"),
                                     via = NULL,
                                     priority = NULL,
                                     include_warnings = TRUE) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  mode <- .depgraph_normalize_constraint_mode(
    .depgraph_match_arg(mode, .depgraph_constraint_modes, "mode")
  )
  strategy <- .depgraph_match_arg(strategy, c("strict", "rule_based"), "strategy")

  result <- switch(
    mode,
    subject = .derive_subject_constraints(graph, samples = samples),
    batch = .derive_batch_constraints(graph, samples = samples),
    study = .derive_study_constraints(graph, samples = samples),
    time = .derive_time_constraints(graph, samples = samples),
    site = .derive_site_constraints(graph, samples = samples),
    region = .derive_region_constraints(graph, samples = samples),
    platform = .derive_platform_constraints(graph, samples = samples),
    assay = .derive_assay_constraints(graph, samples = samples),
    relatedness = .derive_pairwise_constraints(graph, "relatedness", samples = samples),
    spatial = .derive_pairwise_constraints(graph, "spatial", samples = samples),
    composite = if (identical(strategy, "strict")) {
      .derive_composite_strict_constraints(graph, samples = samples, via = via)
    } else {
      .derive_rule_based_constraints(graph, samples = samples, priority = priority, via = via)
    }
  )

  if (!isTRUE(include_warnings)) {
    result$metadata$warnings <- character()
  }

  result
}

#' @rdname derive_split_constraints
#' @export
grouping_vector <- function(x) {
  .depgraph_assert(inherits(x, "split_constraint"), "`x` must be a `split_constraint`.")
  groups <- as.character(x$sample_map$group_id)
  names(groups) <- x$sample_map$sample_id
  groups
}

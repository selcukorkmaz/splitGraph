# Validation helpers for dependency_graph objects.

.depgraph_empty_issue_data <- function() {
  data.frame(
    issue_id = character(),
    level = character(),
    severity = character(),
    code = character(),
    message = character(),
    node_ids = I(list()),
    edge_ids = I(list()),
    details = I(list()),
    stringsAsFactors = FALSE
  )
}

.new_validation_issue <- function(level, severity, code, message, node_ids = character(), edge_ids = character(), details = list()) {
  .depgraph_assert(level %in% .depgraph_validation_levels, paste0("Unsupported validation level: ", level))
  .depgraph_assert(severity %in% .depgraph_validation_severities, paste0("Unsupported validation severity: ", severity))

  data.frame(
    issue_id = NA_character_,
    level = level,
    severity = severity,
    code = code,
    message = as.character(message)[1L],
    node_ids = I(list(as.character(node_ids))),
    edge_ids = I(list(as.character(edge_ids))),
    details = I(list(.depgraph_normalize_attr_entry(details, context = "`details`"))),
    stringsAsFactors = FALSE
  )
}

# Bulk variant of `.new_validation_issue()`: one issue per element of
# `messages`, with per-issue node/edge id vectors supplied as lists. Building the
# table once instead of rbind-ing one-row frames is what keeps validation linear
# on cohorts with thousands of per-subject advisories.
.new_validation_issues <- function(level, severity, code, messages, node_ids = NULL, edge_ids = NULL, details = NULL) {
  n <- length(messages)
  if (n == 0L) {
    return(NULL)
  }
  .depgraph_assert(level %in% .depgraph_validation_levels, paste0("Unsupported validation level: ", level))
  .depgraph_assert(severity %in% .depgraph_validation_severities, paste0("Unsupported validation severity: ", severity))

  node_ids <- if (is.null(node_ids)) rep(list(character()), n) else lapply(node_ids, as.character)
  edge_ids <- if (is.null(edge_ids)) rep(list(character()), n) else lapply(edge_ids, as.character)
  details <- if (is.null(details)) rep(list(list()), n) else details
  details <- lapply(details, .depgraph_normalize_attr_entry, context = "`details`")

  data.frame(
    issue_id = NA_character_,
    level = level,
    severity = severity,
    code = code,
    message = as.character(messages),
    node_ids = I(unname(node_ids)),
    edge_ids = I(unname(edge_ids)),
    details = I(unname(details)),
    stringsAsFactors = FALSE
  )
}

.depgraph_bind_issues <- function(issues) {
  issues <- Filter(Negate(is.null), issues)
  if (length(issues) == 0L) {
    return(.depgraph_empty_issue_data())
  }

  out <- do.call(rbind, issues)
  row.names(out) <- NULL
  if (nrow(out) == 0L) {
    return(.depgraph_empty_issue_data())
  }
  out$issue_id <- paste0("issue_", seq_len(nrow(out)))
  out
}

.depgraph_issue_count_table <- function(x, column) {
  if (nrow(x) == 0L) {
    return(data.frame(value = character(), n = integer(), stringsAsFactors = FALSE))
  }
  .depgraph_count_table(x[[column]])
}

.summarize_validation_issues <- function(issues) {
  list(
    n_issues = nrow(issues),
    by_level = .depgraph_issue_count_table(issues, "level"),
    by_severity = .depgraph_issue_count_table(issues, "severity"),
    by_code = .depgraph_issue_count_table(issues, "code")
  )
}

.validate_structural <- function(graph) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  issues <- list()

  alignment <- .depgraph_graph_table_alignment(graph$graph, node_data, edge_data)
  if (!isTRUE(alignment$valid)) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "igraph_table_mismatch",
      message = alignment$message
    )
  }

  if (anyNA(node_data$node_id) || any(!nzchar(node_data$node_id))) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_node_id",
      message = "Node table contains missing or empty `node_id` values."
    )
  }

  if (anyDuplicated(node_data$node_id) > 0L) {
    dup_ids <- unique(node_data$node_id[duplicated(node_data$node_id)])
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "duplicate_node_id",
      message = paste0("Node table contains duplicated `node_id` values: ", paste(dup_ids, collapse = ", ")),
      node_ids = dup_ids
    )
  }

  if (anyNA(edge_data$edge_id) || any(!nzchar(edge_data$edge_id))) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_edge_id",
      message = "Edge table contains missing or empty `edge_id` values."
    )
  }

  if (anyDuplicated(edge_data$edge_id) > 0L) {
    dup_ids <- unique(edge_data$edge_id[duplicated(edge_data$edge_id)])
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "duplicate_edge_id",
      message = paste0("Edge table contains duplicated `edge_id` values: ", paste(dup_ids, collapse = ", ")),
      edge_ids = dup_ids
    )
  }

  if (nrow(edge_data) > 0L) {
    triplets <- paste(edge_data$from, edge_data$to, edge_data$edge_type, sep = "||")
    if (anyDuplicated(triplets) > 0L) {
      dup_rows <- duplicated(triplets) | duplicated(triplets, fromLast = TRUE)
      dup_edges <- edge_data$edge_id[dup_rows]
      issues[[length(issues) + 1L]] <- .new_validation_issue(
        level = "structural",
        severity = "error",
        code = "duplicate_edge_triplet",
        message = "Edge table contains duplicate `(from, to, edge_type)` relations.",
        edge_ids = dup_edges
      )
    }
  }

  valid_nodes <- node_data$node_id
  bad_from <- setdiff(unique(edge_data$from), valid_nodes)
  bad_to <- setdiff(unique(edge_data$to), valid_nodes)
  if (length(bad_from) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_source_node",
      message = paste0("Edges reference missing source nodes: ", paste(bad_from, collapse = ", ")),
      node_ids = bad_from
    )
  }
  if (length(bad_to) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_target_node",
      message = paste0("Edges reference missing target nodes: ", paste(bad_to, collapse = ", ")),
      node_ids = bad_to
    )
  }

  bad_node_types <- setdiff(unique(node_data$node_type), .depgraph_node_types)
  if (length(bad_node_types) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "unsupported_node_type",
      message = paste0("Unsupported node types found: ", paste(bad_node_types, collapse = ", "))
    )
  }

  if (nrow(edge_data) > 0L) {
    # Vectorised signature check: one join of the edge table against the schema.
    type_map <- stats::setNames(node_data$node_type, node_data$node_id)
    schema_idx <- match(edge_data$edge_type, .depgraph_edge_schema$edge_type)
    unknown <- is.na(schema_idx)

    if (any(unknown)) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "structural",
        severity = "error",
        code = "unsupported_edge_type",
        messages = paste0("Unknown edge type encountered: ", edge_data$edge_type[unknown]),
        edge_ids = as.list(edge_data$edge_id[unknown])
      )
    }

    known <- which(!unknown)
    if (length(known) > 0L) {
      expected_from <- .depgraph_edge_schema$from_type[schema_idx[known]]
      expected_to <- .depgraph_edge_schema$to_type[schema_idx[known]]
      observed_from <- unname(type_map[edge_data$from[known]])
      observed_to <- unname(type_map[edge_data$to[known]])
      # An endpoint missing from the node table (reported separately above)
      # compares as NA; treat it as a mismatch, as the per-edge loop did.
      bad <- is.na(observed_from) | is.na(observed_to) |
        observed_from != expected_from | observed_to != expected_to
      bad_idx <- known[bad]
      if (length(bad_idx) > 0L) {
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "structural",
          severity = "error",
          code = "invalid_edge_signature",
          messages = paste0(
            "Edge `", edge_data$edge_id[bad_idx], "` violates schema for `", edge_data$edge_type[bad_idx],
            "` (expected ", expected_from[bad], " -> ", expected_to[bad],
            ", found ", observed_from[bad], " -> ", observed_to[bad], ")."
          ),
          node_ids = Map(c, edge_data$from[bad_idx], edge_data$to[bad_idx]),
          edge_ids = as.list(edge_data$edge_id[bad_idx])
        )
      }
    }
  }

  for (edge_type in .depgraph_single_target_edges) {
    subset_idx <- edge_data$edge_type == edge_type
    if (!any(subset_idx)) {
      next
    }
    edge_subset <- edge_data[subset_idx, c("edge_id", "from", "to"), drop = FALSE]
    counts <- .depgraph_count_unique_by(edge_subset$to, edge_subset$from)
    bad <- data.frame(from = counts$by[counts$n > 1L], stringsAsFactors = FALSE)
    if (nrow(bad) > 0L) {
      offending_edges <- edge_subset$edge_id[edge_subset$from %in% bad$from]
      issues[[length(issues) + 1L]] <- .new_validation_issue(
        level = "structural",
        severity = "error",
        code = "single_target_violation",
        message = paste0(
          "Edge type `", edge_type, "` links a source node to multiple targets: ",
          paste(bad$from, collapse = ", ")
        ),
        node_ids = bad$from,
        edge_ids = offending_edges,
        details = list(edge_type = edge_type)
      )
    }
  }

  .depgraph_bind_issues(issues)
}

.validate_semantic <- function(graph) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  issues <- list()

  if (nrow(node_data) > 0L) {
    # Only nodes that carry attributes can have unknown ones, and no node type
    # currently declares required attributes; still evaluate the required check
    # for every node type present (once per type, not once per node) so a
    # future schema with required attrs keeps working.
    has_attrs <- lengths(node_data$attrs) > 0L
    required_by_type <- vapply(
      unique(node_data$node_type),
      function(type) length(.depgraph_node_type_schema(type)$required_attrs) > 0L,
      logical(1)
    )
    needs_check <- has_attrs | required_by_type[node_data$node_type]

    checks <- vector("list", nrow(node_data))
    checks[needs_check] <- Map(
      function(type, attrs) .depgraph_validate_node_attrs(type, attrs),
      node_data$node_type[needs_check], node_data$attrs[needs_check]
    )
    missing_required <- lapply(checks, function(x) if (is.null(x)) character() else x$missing_required)
    unknown_attrs <- lapply(checks, function(x) if (is.null(x)) character() else x$unknown_attrs)

    miss_idx <- which(lengths(missing_required) > 0L)
    if (length(miss_idx) > 0L) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "semantic",
        severity = "error",
        code = "missing_required_node_attr",
        messages = paste0(
          "Node `", node_data$node_id[miss_idx], "` is missing required attributes: ",
          vapply(missing_required[miss_idx], paste, character(1), collapse = ", ")
        ),
        node_ids = as.list(node_data$node_id[miss_idx])
      )
    }

    unk_idx <- which(has_attrs & lengths(unknown_attrs) > 0L)
    if (length(unk_idx) > 0L) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "semantic",
        severity = "warning",
        code = "non_canonical_node_attr",
        messages = paste0(
          "Node `", node_data$node_id[unk_idx], "` contains non-canonical attributes: ",
          vapply(unknown_attrs[unk_idx], paste, character(1), collapse = ", ")
        ),
        node_ids = as.list(node_data$node_id[unk_idx])
      )
    }
  }

  sample_ids <- node_data$node_id[node_data$node_type == "Sample"]
  for (relation in names(.depgraph_semantic_relation_rules)) {
    rule <- .depgraph_semantic_relation_rules[[relation]]
    schema_row <- .depgraph_edge_type_schema(relation)
    relation_edges <- edge_data[edge_data$edge_type == relation, c("edge_id", "from", "to"), drop = FALSE]

    counts <- stats::setNames(integer(length(sample_ids)), sample_ids)
    targets <- vector("list", length(sample_ids))
    names(targets) <- sample_ids

    if (nrow(relation_edges) > 0L) {
      targets_by_sample <- lapply(split(relation_edges$to, relation_edges$from), unique)
      counts[names(targets_by_sample)] <- lengths(targets_by_sample)
      targets[names(targets_by_sample)] <- targets_by_sample
    }

    if (!is.null(rule$missing_code) && !is.null(rule$missing_severity) && rule$min_targets > 0L) {
      target_type_present <- nrow(schema_row) == 1L &&
        any(node_data$node_type == schema_row$to_type[[1L]])
      if (nrow(relation_edges) > 0L || target_type_present) {
        missing_samples <- names(counts)[counts < rule$min_targets]
        if (length(missing_samples) > 0L) {
          issues[[length(issues) + 1L]] <- .new_validation_issue(
            level = "semantic",
            severity = rule$missing_severity,
            code = rule$missing_code,
            message = paste0(
              "Samples missing required relation `", relation, "`: ",
              paste(missing_samples, collapse = ", ")
            ),
            node_ids = missing_samples,
            details = list(edge_type = relation)
          )
        }
      }
    }

    allow_multiple <- identical(relation, "sample_belongs_to_subject") &&
      .depgraph_validation_override(graph, "allow_multi_subject_samples", default = FALSE)

    if (!allow_multiple && !is.null(rule$multiple_code) && !is.null(rule$multiple_severity) && is.finite(rule$max_targets)) {
      multi_samples <- names(counts)[counts > rule$max_targets]
      if (length(multi_samples) > 0L) {
        edge_ids <- relation_edges$edge_id[relation_edges$from %in% multi_samples]
        related_nodes <- unique(c(
          multi_samples,
          unlist(targets[multi_samples], use.names = FALSE)
        ))
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = rule$multiple_severity,
          code = rule$multiple_code,
          message = paste0(
            "Samples violate relation cardinality for `", relation, "`: ",
            paste(multi_samples, collapse = ", ")
          ),
          node_ids = related_nodes,
          edge_ids = edge_ids,
          details = list(edge_type = relation)
        )
      }
    }
  }

  feature_nodes <- node_data[node_data$node_type == "FeatureSet", , drop = FALSE]
  if (nrow(feature_nodes) > 0L) {
    for (i in seq_len(nrow(feature_nodes))) {
      derivation_scope <- .depgraph_extract_attr(feature_nodes$attrs[[i]], "derivation_scope")
      if (!is.na(derivation_scope) && !derivation_scope %in% .depgraph_allowed_derivation_scopes) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = "warning",
          code = "invalid_featureset_derivation_scope",
          message = paste0(
            "FeatureSet node `", feature_nodes$node_id[[i]], "` has invalid `derivation_scope`: ",
            derivation_scope
          ),
          node_ids = feature_nodes$node_id[[i]]
        )
      }
    }
  }

  outcome_nodes <- node_data[node_data$node_type == "Outcome", , drop = FALSE]
  if (nrow(outcome_nodes) > 0L) {
    for (i in seq_len(nrow(outcome_nodes))) {
      observation_level <- .depgraph_extract_attr(outcome_nodes$attrs[[i]], "observation_level")
      if (!is.na(observation_level) && !observation_level %in% .depgraph_allowed_observation_levels) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = "warning",
          code = "invalid_outcome_observation_level",
          message = paste0(
            "Outcome node `", outcome_nodes$node_id[[i]], "` has invalid `observation_level`: ",
            observation_level
          ),
          node_ids = outcome_nodes$node_id[[i]]
        )
      }
    }
  }

  time_nodes <- node_data[node_data$node_type == "Timepoint", , drop = FALSE]
  if (nrow(time_nodes) > 0L) {
    for (i in seq_len(nrow(time_nodes))) {
      value <- .depgraph_extract_attr(time_nodes$attrs[[i]], "time_index")
      converted <- suppressWarnings(as.numeric(value))
      if (!is.na(value) && is.na(converted)) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = "warning",
          code = "invalid_time_index",
          message = paste0("Timepoint node `", time_nodes$node_id[[i]], "` has a non-numeric `time_index`."),
          node_ids = time_nodes$node_id[[i]]
        )
      }
    }
  }

  precedes <- edge_data[edge_data$edge_type == "timepoint_precedes", , drop = FALSE]
  if (nrow(precedes) > 0L && any(precedes$from == precedes$to)) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "semantic",
      severity = "error",
      code = "timepoint_self_loop",
      message = "`timepoint_precedes` contains self-loops.",
      edge_ids = precedes$edge_id[precedes$from == precedes$to]
    )
  }

  time_consistency <- .depgraph_time_order_consistency(node_data, edge_data)
  if (length(time_consistency$cycle_edge_ids) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "semantic",
      severity = "error",
      code = "timepoint_precedence_cycle",
      message = "`timepoint_precedes` must define an acyclic ordering.",
      edge_ids = time_consistency$cycle_edge_ids
    )
  }
  if (nrow(time_consistency$conflicting_pairs) > 0L) {
    conflicting_nodes <- unique(c(
      time_consistency$conflicting_pairs$from,
      time_consistency$conflicting_pairs$to
    ))
    pair_labels <- paste0(
      time_consistency$conflicting_pairs$from,
      " -> ",
      time_consistency$conflicting_pairs$to
    )
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "semantic",
      severity = "error",
      code = "time_order_conflict",
      message = paste0(
        "Timepoint ordering metadata conflict between `time_index` and `timepoint_precedes`: ",
        paste(pair_labels, collapse = ", ")
      ),
      node_ids = conflicting_nodes,
      edge_ids = time_consistency$conflicting_pairs$edge_id
    )
  }

  .depgraph_bind_issues(issues)
}

.validate_leakage <- function(graph) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  issues <- list()

  subject_edges <- edge_data[edge_data$edge_type == "sample_belongs_to_subject", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(subject_edges) > 0L) {
    sample_counts <- .depgraph_count_unique_by(subject_edges$from, subject_edges$to)
    subject_ids <- sample_counts$by[sample_counts$n > 1L]
    if (length(subject_ids) > 0L) {
      samples_by_subject <- split(subject_edges$from, subject_edges$to)
      edges_by_subject <- split(subject_edges$edge_id, subject_edges$to)
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "advisory",
        code = "repeated_subject_samples",
        messages = paste0("Subject `", subject_ids, "` is linked to multiple samples."),
        node_ids = Map(function(s, samples) c(s, unique(samples)), subject_ids, samples_by_subject[subject_ids]),
        edge_ids = edges_by_subject[subject_ids]
      )
    }
  }

  study_edges <- edge_data[edge_data$edge_type == "sample_from_study", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(subject_edges) > 0L && nrow(study_edges) > 0L) {
    subject_study <- merge(
      subject_edges[, c("from", "to")],
      study_edges[, c("from", "to")],
      by = "from",
      suffixes = c("_subject", "_study")
    )
    if (nrow(subject_study) > 0L) {
      study_counts <- .depgraph_count_unique_by(subject_study$to_study, subject_study$to_subject)
      subject_ids <- study_counts$by[study_counts$n > 1L]
      if (length(subject_ids) > 0L) {
        samples_by_subject <- split(subject_study$from, subject_study$to_subject)
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "leakage",
          severity = "warning",
          code = "subject_cross_study_overlap",
          messages = paste0("Subject `", subject_ids, "` appears across multiple studies."),
          node_ids = Map(function(s, samples) c(s, unique(samples)), subject_ids, samples_by_subject[subject_ids])
        )
      }
    }
  }

  # A subject whose samples were collected at several sites: mirrors the
  # cross-study rule. Grouping by site alone would then place one individual
  # in several site groups, and a site holdout would leak that individual.
  site_edges <- edge_data[edge_data$edge_type == "sample_collected_at_site", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(subject_edges) > 0L && nrow(site_edges) > 0L) {
    subject_site <- merge(
      subject_edges[, c("from", "to")],
      site_edges[, c("from", "to")],
      by = "from",
      suffixes = c("_subject", "_site")
    )
    if (nrow(subject_site) > 0L) {
      site_counts <- .depgraph_count_unique_by(subject_site$to_site, subject_site$to_subject)
      subject_ids <- site_counts$by[site_counts$n > 1L]
      if (length(subject_ids) > 0L) {
        samples_by_subject <- split(subject_site$from, subject_site$to_subject)
        sites_by_subject <- split(subject_site$to_site, subject_site$to_subject)
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "leakage",
          severity = "warning",
          code = "subject_cross_site_overlap",
          messages = paste0("Subject `", subject_ids, "` has samples collected at multiple sites."),
          node_ids = Map(
            function(s, samples, sites) c(s, unique(samples), unique(sites)),
            subject_ids, samples_by_subject[subject_ids], sites_by_subject[subject_ids]
          )
        )
      }
    }
  }

  feature_nodes <- node_data[node_data$node_type == "FeatureSet", , drop = FALSE]
  if (nrow(feature_nodes) > 0L) {
    for (i in seq_len(nrow(feature_nodes))) {
      derivation_scope <- .depgraph_extract_attr(feature_nodes$attrs[[i]], "derivation_scope")
      if (!is.na(derivation_scope) && identical(derivation_scope, "per_dataset")) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "leakage",
          severity = "advisory",
          code = "per_dataset_featureset",
          message = paste0("FeatureSet `", feature_nodes$node_id[[i]], "` was derived at the full-dataset scope."),
          node_ids = feature_nodes$node_id[[i]]
        )
      }
    }
  }

  feature_edges <- edge_data[edge_data$edge_type == "sample_uses_featureset", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(feature_edges) > 0L) {
    feature_counts <- .depgraph_count_unique_by(feature_edges$from, feature_edges$to)
    fs_ids <- feature_counts$by[feature_counts$n > 1L]
    if (length(fs_ids) > 0L) {
      samples_by_fs <- split(feature_edges$from, feature_edges$to)
      edges_by_fs <- split(feature_edges$edge_id, feature_edges$to)
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "advisory",
        code = "shared_featureset_provenance",
        messages = paste0("FeatureSet `", fs_ids, "` is shared across multiple samples."),
        node_ids = Map(function(f, samples) c(f, unique(samples)), fs_ids, samples_by_fs[fs_ids]),
        edge_ids = edges_by_fs[fs_ids]
      )
    }
  }

  has_time_edges <- any(edge_data$edge_type == "sample_collected_at_timepoint")
  if (has_time_edges) {
    has_precedes <- any(edge_data$edge_type == "timepoint_precedes")
    time_nodes <- node_data[node_data$node_type == "Timepoint", , drop = FALSE]
    time_indices <- vapply(
      time_nodes$attrs,
      function(x) {
        value <- .depgraph_extract_attr(x, "time_index")
        if (length(value) == 0L || is.na(value)) {
          return(NA_character_)
        }
        as.character(value)
      },
      FUN.VALUE = character(1)
    )
    if (!has_precedes && (length(time_indices) == 0L || all(is.na(time_indices)))) {
      issues[[length(issues) + 1L]] <- .new_validation_issue(
        level = "leakage",
        severity = "warning",
        code = "missing_time_ordering",
        message = "Timepoint-linked samples exist but no explicit time ordering metadata were found."
      )
    }
  }

  batch_edges <- edge_data[edge_data$edge_type == "sample_processed_in_batch", c("edge_id", "from", "to"), drop = FALSE]
  n_samples <- sum(node_data$node_type == "Sample")
  if (nrow(batch_edges) > 0L && n_samples > 0L) {
    batch_counts <- .depgraph_count_unique_by(batch_edges$from, batch_edges$to)
    threshold <- max(3L, ceiling(n_samples * 0.5))
    batch_ids <- batch_counts$by[batch_counts$n >= threshold]
    if (length(batch_ids) > 0L) {
      edges_by_batch <- split(batch_edges$edge_id, batch_edges$to)
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "advisory",
        code = "heavy_batch_reuse",
        messages = paste0("Batch `", batch_ids, "` is reused across many samples."),
        node_ids = as.list(batch_ids),
        edge_ids = edges_by_batch[batch_ids]
      )
    }
  }

  .depgraph_bind_issues(issues)
}

#' @rdname build_dependency_graph
#' @export
validate_graph <- function(graph, error_on_fail = FALSE, levels = NULL, severities = NULL, validation_overrides = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")

  if (!is.null(validation_overrides)) {
    .depgraph_assert(
      is.list(validation_overrides) && (length(validation_overrides) == 0L ||
        (!is.null(names(validation_overrides)) &&
           all(nzchar(names(validation_overrides))))),
      "`validation_overrides` must be a named list."
    )
    graph <- .depgraph_with_overrides(graph, validation_overrides)
  }

  selected_levels <- if (is.null(levels)) .depgraph_validation_levels else unique(as.character(levels))
  .depgraph_assert(all(selected_levels %in% .depgraph_validation_levels), "`levels` contains unsupported values.")

  selected_severities <- if (is.null(severities)) .depgraph_validation_severities else unique(as.character(severities))
  .depgraph_assert(all(selected_severities %in% .depgraph_validation_severities), "`severities` contains unsupported values.")

  issue_sets <- list()
  if ("structural" %in% selected_levels) {
    issue_sets[[length(issue_sets) + 1L]] <- .validate_structural(graph)
  }
  if ("semantic" %in% selected_levels) {
    issue_sets[[length(issue_sets) + 1L]] <- .validate_semantic(graph)
  }
  if ("leakage" %in% selected_levels) {
    issue_sets[[length(issue_sets) + 1L]] <- .validate_leakage(graph)
  }

  all_issues <- .depgraph_bind_issues(issue_sets)
  issues <- all_issues
  if (nrow(issues) > 0L) {
    issues <- issues[issues$severity %in% selected_severities, , drop = FALSE]
    row.names(issues) <- NULL
  }

  all_errors <- unique(all_issues$message[all_issues$severity == "error"])
  all_warnings <- unique(all_issues$message[all_issues$severity == "warning"])
  all_advisories <- unique(all_issues$message[all_issues$severity == "advisory"])

  metrics <- list(
    n_nodes = nrow(graph$nodes$data),
    n_edges = nrow(graph$edges$data),
    n_samples = sum(graph$nodes$data$node_type == "Sample"),
    n_issues = nrow(all_issues),
    n_errors = length(all_errors),
    n_warnings = length(all_warnings),
    n_advisories = length(all_advisories)
  )

  report <- depgraph_validation_report(
    graph_name = graph$metadata$graph_name,
    issues = issues,
    metrics = metrics,
    metadata = list(
      levels = selected_levels,
      severities = selected_severities
    ),
    valid = length(all_errors) == 0L,
    errors = all_errors,
    warnings = all_warnings,
    advisories = all_advisories
  )

  if (length(all_errors) > 0L && isTRUE(error_on_fail)) {
    .depgraph_stop(
      paste(c("Graph validation failed.", report$errors), collapse = "\n"),
      class = "splitgraph_validation_error",
      code = "graph_validation_failed"
    )
  }

  report
}

# Helpers for translating splitGraph constraints into stable, tool-agnostic
# sample-level split specifications. Downstream packages (bioLeak, fastml,
# rsample, ...) consume `split_spec` objects through their own adapters.

.split_spec_sample_data_template <- function(n = 0L) {
  data.frame(
    sample_id = rep(NA_character_, n),
    sample_node_id = rep(NA_character_, n),
    group_id = rep(NA_character_, n),
    primary_group = rep(NA_character_, n),
    batch_group = rep(NA_character_, n),
    study_group = rep(NA_character_, n),
    timepoint_id = rep(NA_character_, n),
    time_index = rep(NA_real_, n),
    order_rank = rep(NA_integer_, n),
    stringsAsFactors = FALSE
  )
}

.split_spec_new_issue <- function(severity, code, message, n_affected = 0L, details = list()) {
  data.frame(
    issue_id = NA_character_,
    severity = as.character(severity)[1L],
    code = as.character(code)[1L],
    message = as.character(message)[1L],
    n_affected = as.integer(n_affected)[1L],
    details = I(list(details)),
    stringsAsFactors = FALSE
  )
}

.split_spec_bind_issues <- function(issues) {
  issues <- Filter(Negate(is.null), issues)
  if (length(issues) == 0L) {
    return(data.frame(
      issue_id = character(),
      severity = character(),
      code = character(),
      message = character(),
      n_affected = integer(),
      details = I(list()),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, issues)
  row.names(out) <- NULL
  out$issue_id <- paste0("split_spec_issue_", seq_len(nrow(out)))
  out
}

.split_spec_resampling_hint <- function(mode, strategy = NULL) {
  mode <- tolower(as.character(mode)[1L])
  strategy <- if (is.null(strategy)) NULL else tolower(as.character(strategy)[1L])

  if (identical(mode, "subject")) return("grouped_cv")
  if (identical(mode, "batch")) return("blocked_cv")
  if (identical(mode, "study")) return("leave_one_group_out")
  if (identical(mode, "time")) return("ordered_split")
  if (identical(mode, "composite") && identical(strategy, "strict")) return("custom_grouped_cv")
  if (identical(mode, "composite") && identical(strategy, "rule_based")) return("grouped_cv")
  "grouped_cv"
}

.split_spec_match_assignment_key <- function(assignments, sample_node_ids) {
  matched <- assignments[match(sample_node_ids, assignments$sample_node_id), , drop = FALSE]
  matched$linked_key
}

.split_spec_enrich_from_graph <- function(sample_data, graph) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  sample_ids <- sample_data$sample_node_id

  batch_assignments <- .depgraph_direct_assignment(graph, "batch", samples = sample_ids)
  study_assignments <- .depgraph_direct_assignment(graph, "study", samples = sample_ids)
  time_constraint <- .derive_time_constraints(graph, samples = sample_ids)$sample_map
  time_constraint <- time_constraint[match(sample_ids, time_constraint$sample_node_id), , drop = FALSE]

  missing_batch <- is.na(sample_data$batch_group) | !nzchar(sample_data$batch_group)
  sample_data$batch_group[missing_batch] <- .split_spec_match_assignment_key(batch_assignments, sample_ids)[missing_batch]

  missing_study <- is.na(sample_data$study_group) | !nzchar(sample_data$study_group)
  sample_data$study_group[missing_study] <- .split_spec_match_assignment_key(study_assignments, sample_ids)[missing_study]

  missing_timepoint <- is.na(sample_data$timepoint_id) | !nzchar(sample_data$timepoint_id)
  sample_data$timepoint_id[missing_timepoint] <- time_constraint$timepoint_id[missing_timepoint]

  missing_time_index <- is.na(sample_data$time_index)
  sample_data$time_index[missing_time_index] <- time_constraint$time_index[missing_time_index]

  missing_order <- is.na(sample_data$order_rank)
  sample_data$order_rank[missing_order] <- time_constraint$order_rank[missing_order]

  sample_data
}

.split_spec_constraint_summary <- function(constraint) {
  if (is.null(constraint)) {
    return(list())
  }

  sample_map <- constraint$sample_map
  group_counts <- table(sample_map$group_id)
  list(
    mode = constraint$metadata$mode %||% NA_character_,
    strategy = constraint$metadata$strategy %||% constraint$strategy,
    n_samples = nrow(sample_map),
    n_groups = length(group_counts),
    n_singleton_groups = sum(group_counts == 1L),
    warnings = constraint$metadata$warnings %||% character()
  )
}

#' Translate splitGraph Constraints into Stable Split Specifications
#'
#' Translate graph-derived split constraints into a stable, inspectable
#' structure for sample-level grouping, blocking, and ordering, perform
#' preflight structural checks on that translation, and summarize structural
#' leakage risks.
#'
#' The translation layer always produces canonical sample-level columns
#' including \code{sample_id}, \code{sample_node_id}, \code{group_id}, and
#' \code{primary_group}. When available, it also carries \code{batch_group},
#' \code{study_group}, \code{timepoint_id}, \code{time_index}, and
#' \code{order_rank}. Missing but relevant fields are retained as \code{NA}
#' columns rather than omitted.
#'
#' When only a subset of samples has ordering metadata, the translated split
#' spec still exposes that partial ordering through \code{time_var}, but
#' \code{ordering_required} remains \code{FALSE}. Ordering is only marked as
#' required when the constraint implies complete ordering coverage.
#'
#' The split-spec validator checks:
#' \itemize{
#'   \item missing required columns
#'   \item missing or duplicated sample identifiers
#'   \item missing grouping assignments
#'   \item singleton-only grouping structures
#'   \item missing ordering when ordering is required
#'   \item invalid or empty block variables
#' }
#'
#' Repeated validation of the same split spec yields deterministic issue IDs
#' and diagnostics, which makes the returned validation object stable across
#' runs.
#'
#' The produced \code{split_spec} is tool-agnostic. Downstream consumers are
#' expected to provide their own adapters to convert a \code{split_spec} into
#' their native split representation, so \pkg{splitGraph} has no runtime
#' dependency on any of them.
#'
#' \code{summarize_leakage_risks()} reuses \code{validate_graph()} and
#' \code{split_constraint} metadata rather than duplicating downstream
#' evaluation logic.
#'
#' @param constraint A \code{split_constraint}.
#' @param graph A \code{dependency_graph}.
#' @param split_spec An optional \code{split_spec}.
#' @param validation An optional \code{depgraph_validation_report}.
#' @param x A \code{split_spec}.
#' @return \code{as_split_spec()} returns a \code{split_spec}.
#'   \code{validate_split_spec()} returns a \code{split_spec_validation}.
#'   \code{summarize_leakage_risks()} returns a \code{leakage_risk_summary}.
#' @examples
#' meta <- data.frame(
#'   sample_id  = c("S1", "S2", "S3", "S4"),
#'   subject_id = c("P1", "P1", "P2", "P2")
#' )
#' g <- graph_from_metadata(meta)
#'
#' constraint <- derive_split_constraints(g, mode = "subject")
#' spec <- as_split_spec(constraint, graph = g)
#' validate_split_spec(spec)
#' summarize_leakage_risks(g, constraint = constraint, split_spec = spec)
#' @export
as_split_spec <- function(constraint, graph = NULL) {
  .depgraph_assert(inherits(constraint, "split_constraint"), "`constraint` must be a `split_constraint`.")

  sample_map <- constraint$sample_map
  sample_data <- .split_spec_sample_data_template(nrow(sample_map))
  sample_data$sample_id <- as.character(sample_map$sample_id)
  sample_data$sample_node_id <- as.character(sample_map$sample_node_id)
  sample_data$group_id <- as.character(sample_map$group_id)
  sample_data$primary_group <- sample_data$group_id

  mode <- constraint$metadata$mode %||% NA_character_
  strategy <- constraint$metadata$strategy %||% constraint$strategy

  if (identical(mode, "batch")) {
    sample_data$batch_group <- as.character(sample_map$group_label)
  }
  if (identical(mode, "study")) {
    sample_data$study_group <- as.character(sample_map$group_label)
  }

  if ("timepoint_id" %in% names(sample_map)) {
    sample_data$timepoint_id <- as.character(sample_map$timepoint_id)
  }
  if ("time_index" %in% names(sample_map)) {
    sample_data$time_index <- suppressWarnings(as.numeric(sample_map$time_index))
  }
  if ("order_rank" %in% names(sample_map)) {
    sample_data$order_rank <- suppressWarnings(as.integer(sample_map$order_rank))
  }

  enrichment_used <- FALSE
  if (!is.null(graph)) {
    sample_data <- .split_spec_enrich_from_graph(sample_data, graph)
    enrichment_used <- TRUE
  }

  block_vars <- c()
  if (!all(is.na(sample_data$batch_group))) {
    block_vars <- c(block_vars, "batch_group")
  }
  if (!all(is.na(sample_data$study_group))) {
    block_vars <- c(block_vars, "study_group")
  }

  time_var <- if (!all(is.na(sample_data$order_rank))) "order_rank" else NULL
  ordering_required <- isTRUE(constraint$recommended_downstream_args$ordering_required)

  split_spec(
    sample_data = sample_data,
    group_var = "group_id",
    block_vars = block_vars,
    time_var = time_var,
    ordering_required = ordering_required,
    constraint_mode = mode,
    constraint_strategy = strategy,
    recommended_resampling = .split_spec_resampling_hint(mode, strategy),
    metadata = list(
      graph_name = if (is.null(graph)) NULL else graph$metadata$graph_name,
      dataset_name = if (is.null(graph)) NULL else graph$metadata$dataset_name,
      source_mode = mode,
      source_strategy = strategy,
      relations_used = constraint$metadata$relations_used %||% character(),
      n_samples = nrow(sample_data),
      n_groups = length(unique(sample_data$group_id)),
      warnings = constraint$metadata$warnings %||% character(),
      enriched_from_graph = enrichment_used
    )
  )
}

#' @rdname as_split_spec
#' @export
validate_split_spec <- function(x) {
  .depgraph_assert(inherits(x, "split_spec"), "`x` must be a `split_spec`.")
  issues <- list()
  data <- x$sample_data

  required_cols <- c("sample_id", "sample_node_id", x$group_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    issues[[length(issues) + 1L]] <- .split_spec_new_issue(
      severity = "error",
      code = "missing_required_columns",
      message = paste0("Split spec is missing required columns: ", paste(missing_cols, collapse = ", ")),
      n_affected = length(missing_cols),
      details = list(columns = missing_cols)
    )
  }

  if ("sample_id" %in% names(data)) {
    if (any(is.na(data$sample_id) | !nzchar(as.character(data$sample_id)))) {
      issues[[length(issues) + 1L]] <- .split_spec_new_issue(
        severity = "error",
        code = "missing_sample_id",
        message = "Split spec contains missing `sample_id` values.",
        n_affected = sum(is.na(data$sample_id) | !nzchar(as.character(data$sample_id)))
      )
    }

    dup_sample_ids <- unique(data$sample_id[duplicated(data$sample_id)])
    if (length(dup_sample_ids) > 0L) {
      issues[[length(issues) + 1L]] <- .split_spec_new_issue(
        severity = "error",
        code = "duplicate_sample_id",
        message = paste0("Split spec contains duplicated sample IDs: ", paste(dup_sample_ids, collapse = ", ")),
        n_affected = length(dup_sample_ids),
        details = list(sample_ids = dup_sample_ids)
      )
    }
  }

  if (x$group_var %in% names(data)) {
    group_values <- data[[x$group_var]]
    if (any(is.na(group_values) | !nzchar(as.character(group_values)))) {
      issues[[length(issues) + 1L]] <- .split_spec_new_issue(
        severity = "error",
        code = "missing_group_id",
        message = paste0("Split spec contains missing values in `", x$group_var, "`."),
        n_affected = sum(is.na(group_values) | !nzchar(as.character(group_values)))
      )
    } else {
      group_sizes <- table(group_values)
      if (length(group_sizes) > 0L && all(group_sizes == 1L)) {
        issues[[length(issues) + 1L]] <- .split_spec_new_issue(
          severity = "advisory",
          code = "singleton_groups_only",
          message = "All split groups are singletons; grouping may provide little protection against structural leakage.",
          n_affected = length(group_sizes)
        )
      }
    }
  } else {
    issues[[length(issues) + 1L]] <- .split_spec_new_issue(
      severity = "error",
      code = "invalid_group_var",
      message = paste0("Declared `group_var` is not present in `sample_data`: ", x$group_var)
    )
  }

  if (isTRUE(x$ordering_required)) {
    if (is.null(x$time_var) || !x$time_var %in% names(data)) {
      issues[[length(issues) + 1L]] <- .split_spec_new_issue(
        severity = "error",
        code = "invalid_time_var",
        message = "Ordering is required but the declared `time_var` is missing from `sample_data`."
      )
    } else if (any(is.na(data[[x$time_var]]))) {
      issues[[length(issues) + 1L]] <- .split_spec_new_issue(
        severity = "error",
        code = "missing_required_ordering",
        message = "Ordering is required but some samples are missing ordering values.",
        n_affected = sum(is.na(data[[x$time_var]]))
      )
    }
  }

  if (length(x$block_vars) > 0L) {
    for (block_var in x$block_vars) {
      if (!block_var %in% names(data)) {
        issues[[length(issues) + 1L]] <- .split_spec_new_issue(
          severity = "error",
          code = "invalid_block_var",
          message = paste0("Declared block variable is not present in `sample_data`: ", block_var)
        )
      } else if (all(is.na(data[[block_var]]) | !nzchar(as.character(data[[block_var]])))) {
        issues[[length(issues) + 1L]] <- .split_spec_new_issue(
          severity = "warning",
          code = "empty_block_var",
          message = paste0("Block variable `", block_var, "` is present but empty for all samples.")
        )
      }
    }
  }

  split_spec_validation(
    issues = .split_spec_bind_issues(issues),
    metadata = list(
      n_samples = nrow(data),
      n_groups = if (x$group_var %in% names(data)) length(unique(data[[x$group_var]])) else NA_integer_
    )
  )
}

.leakage_summary_from_validation <- function(validation) {
  if (nrow(validation$issues) == 0L) {
    return(data.frame(
      severity = character(),
      category = character(),
      message = character(),
      source = character(),
      n_affected = integer(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    severity = validation$issues$severity,
    category = validation$issues$code,
    message = validation$issues$message,
    source = "validation",
    n_affected = vapply(validation$issues$node_ids, length, integer(1)),
    stringsAsFactors = FALSE
  )
}

.leakage_summary_from_constraint <- function(constraint) {
  if (is.null(constraint)) {
    return(data.frame(
      severity = character(),
      category = character(),
      message = character(),
      source = character(),
      n_affected = integer(),
      stringsAsFactors = FALSE
    ))
  }

  diagnostics <- list()
  warnings <- constraint$metadata$warnings %||% character()
  if (length(warnings) > 0L) {
    for (warning_msg in warnings) {
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(
        severity = "warning",
        category = "constraint_warning",
        message = warning_msg,
        source = "constraint",
        n_affected = nrow(constraint$sample_map),
        stringsAsFactors = FALSE
      )
    }
  }

  if ("group_id" %in% names(constraint$sample_map)) {
    group_sizes <- table(constraint$sample_map$group_id)
    if (length(group_sizes) > 0L && mean(group_sizes == 1L) > 0.5) {
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(
        severity = "advisory",
        category = "singleton_heavy_constraint",
        message = "The derived split constraint is dominated by singleton groups.",
        source = "constraint",
        n_affected = sum(group_sizes == 1L),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(diagnostics) == 0L) {
    return(data.frame(
      severity = character(),
      category = character(),
      message = character(),
      source = character(),
      n_affected = integer(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, diagnostics)
  row.names(out) <- NULL
  out
}

.leakage_summary_from_split_spec <- function(split_spec) {
  if (is.null(split_spec)) {
    return(list(
      diagnostics = data.frame(
        severity = character(),
        category = character(),
        message = character(),
        source = character(),
        n_affected = integer(),
        stringsAsFactors = FALSE
      ),
      summary = list()
    ))
  }

  validation <- validate_split_spec(split_spec)
  diagnostics <- list()
  if (nrow(validation$issues) == 0L) {
    diagnostics[[length(diagnostics) + 1L]] <- data.frame(
      severity = "advisory",
      category = "split_spec_ready",
      message = "Split spec passed preflight validation.",
      source = "split_spec",
      n_affected = nrow(split_spec$sample_data),
      stringsAsFactors = FALSE
    )
  } else {
    diagnostics[[length(diagnostics) + 1L]] <- data.frame(
      severity = validation$issues$severity,
      category = validation$issues$code,
      message = validation$issues$message,
      source = "split_spec",
      n_affected = validation$issues$n_affected,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(split_spec$time_var) && split_spec$time_var %in% names(split_spec$sample_data)) {
    complete_ordering <- sum(!is.na(split_spec$sample_data[[split_spec$time_var]]))
    diagnostics[[length(diagnostics) + 1L]] <- data.frame(
      severity = "advisory",
      category = "ordering_available",
      message = paste0(
        "Split spec provides ordering through `", split_spec$time_var,
        "` for ", complete_ordering, " of ", nrow(split_spec$sample_data), " samples."
      ),
      source = "split_spec",
      n_affected = complete_ordering,
      stringsAsFactors = FALSE
    )
  }

  if (length(split_spec$block_vars) > 0L) {
    for (block_var in split_spec$block_vars) {
      available <- if (block_var %in% names(split_spec$sample_data)) {
        sum(!is.na(split_spec$sample_data[[block_var]]) & nzchar(as.character(split_spec$sample_data[[block_var]])))
      } else {
        0L
      }
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(
        severity = "advisory",
        category = "blocking_available",
        message = paste0(
          "Split spec provides blocking variable `", block_var,
          "` for ", available, " of ", nrow(split_spec$sample_data), " samples."
        ),
        source = "split_spec",
        n_affected = available,
        stringsAsFactors = FALSE
      )
    }
  }

  group_sizes <- table(split_spec$sample_data[[split_spec$group_var]])
  if (length(group_sizes) > 0L && mean(group_sizes == 1L) > 0.5) {
    diagnostics[[length(diagnostics) + 1L]] <- data.frame(
      severity = "advisory",
      category = "split_spec_singleton_heavy",
      message = "Split spec grouping is dominated by singleton groups.",
      source = "split_spec",
      n_affected = sum(group_sizes == 1L),
      stringsAsFactors = FALSE
    )
  }

  diagnostics <- do.call(rbind, diagnostics)
  row.names(diagnostics) <- NULL

  list(
    diagnostics = diagnostics,
    summary = list(
      valid = validation$valid,
      n_samples = nrow(split_spec$sample_data),
      n_groups = length(unique(split_spec$sample_data[[split_spec$group_var]])),
      recommended_resampling = split_spec$recommended_resampling,
      time_var = split_spec$time_var,
      ordering_required = split_spec$ordering_required,
      block_vars = split_spec$block_vars,
      singleton_groups = if (length(group_sizes) == 0L) 0L else sum(group_sizes == 1L)
    )
  )
}

#' @rdname as_split_spec
#' @export
summarize_leakage_risks <- function(graph, constraint = NULL, split_spec = NULL, validation = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  if (!is.null(constraint)) {
    .depgraph_assert(inherits(constraint, "split_constraint"), "`constraint` must be a `split_constraint`.")
  }
  if (!is.null(split_spec)) {
    .depgraph_assert(inherits(split_spec, "split_spec"), "`split_spec` must be a `split_spec`.")
  }

  validation <- validation %||% validate_graph(graph)
  validation_diag <- .leakage_summary_from_validation(validation)
  constraint_diag <- .leakage_summary_from_constraint(constraint)
  split_spec_info <- .leakage_summary_from_split_spec(split_spec)

  diagnostics <- do.call(rbind, Filter(function(x) is.data.frame(x) && nrow(x) > 0L, list(
    validation_diag,
    constraint_diag,
    split_spec_info$diagnostics
  )))
  if (is.null(diagnostics)) {
    diagnostics <- data.frame(
      severity = character(),
      category = character(),
      message = character(),
      source = character(),
      n_affected = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    row.names(diagnostics) <- NULL
  }

  overview <- if (nrow(diagnostics) == 0L) {
    "No structural leakage risks were detected."
  } else {
    paste0(
      "Detected ", nrow(diagnostics), " structural leakage diagnostics across validation",
      if (!is.null(constraint)) ", constraint" else "",
      if (!is.null(split_spec)) ", and split-spec readiness" else "",
      "."
    )
  }

  leakage_risk_summary(
    overview = overview,
    diagnostics = diagnostics,
    validation_summary = validation$summary,
    constraint_summary = .split_spec_constraint_summary(constraint),
    split_spec_summary = split_spec_info$summary,
    metadata = list(
      graph_name = graph$metadata$graph_name,
      dataset_name = graph$metadata$dataset_name
    )
  )
}

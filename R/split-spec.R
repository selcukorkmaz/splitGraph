# Helpers for translating splitGraph constraints into stable, tool-agnostic
# sample-level split specifications. Downstream consumers read `split_spec`
# objects through their own adapters: bioLeak (`as_leaksplits()`, the reference
# consumer), the shipped Python reader (`inst/python/splitspec`), and ad hoc
# adapters such as the rsample example in the adapter-cookbook vignette.

.split_spec_sample_data_template <- function(n = 0L) {
  data.frame(
    sample_id = rep(NA_character_, n),
    sample_node_id = rep(NA_character_, n),
    group_id = rep(NA_character_, n),
    primary_group = rep(NA_character_, n),
    batch_group = rep(NA_character_, n),
    study_group = rep(NA_character_, n),
    site_group = rep(NA_character_, n),
    region_group = rep(NA_character_, n),
    platform_group = rep(NA_character_, n),
    assay_group = rep(NA_character_, n),
    stratum = rep(NA_character_, n),
    timepoint_id = rep(NA_character_, n),
    time_index = rep(NA_real_, n),
    order_rank = rep(NA_integer_, n),
    stringsAsFactors = FALSE
  )
}

# Stratum annotation per sample: the key of the single Outcome node attached to
# the sample (`sample_has_outcome`), falling back to the single Outcome attached
# to the sample's single subject (`subject_has_outcome`). NA when no outcome is
# attached or when the attachment is not unique.
.split_spec_stratum_from_graph <- function(graph, sample_ids) {
  edge_data <- graph$edges$data
  node_data <- graph$nodes$data
  key_of <- function(node_ids) node_data$node_key[match(node_ids, node_data$node_id)]

  stratum <- stats::setNames(rep(NA_character_, length(sample_ids)), sample_ids)
  if (length(sample_ids) == 0L) return(character())

  direct <- edge_data[edge_data$edge_type == "sample_has_outcome" & edge_data$from %in% sample_ids, c("from", "to"), drop = FALSE]
  ambiguous <- character()
  if (nrow(direct) > 0L) {
    outcomes_by_sample <- lapply(split(direct$to, direct$from), unique)
    unique_outcome <- lengths(outcomes_by_sample) == 1L
    stratum[names(outcomes_by_sample)[unique_outcome]] <- key_of(unlist(outcomes_by_sample[unique_outcome], use.names = FALSE))
    # A sample with several outcomes has no unique stratum; it must stay NA
    # rather than borrow its subject's label.
    ambiguous <- names(outcomes_by_sample)[!unique_outcome]
  }

  still_missing <- setdiff(names(stratum)[is.na(stratum)], ambiguous)
  if (length(still_missing) > 0L) {
    belongs <- edge_data[edge_data$edge_type == "sample_belongs_to_subject" & edge_data$from %in% still_missing, c("from", "to"), drop = FALSE]
    subject_outcomes <- edge_data[edge_data$edge_type == "subject_has_outcome", c("from", "to"), drop = FALSE]
    if (nrow(belongs) > 0L && nrow(subject_outcomes) > 0L) {
      outcomes_by_subject <- lapply(split(subject_outcomes$to, subject_outcomes$from), unique)
      unique_subject_outcome <- lengths(outcomes_by_subject) == 1L
      subject_key <- stats::setNames(
        key_of(unlist(outcomes_by_subject[unique_subject_outcome], use.names = FALSE)),
        names(outcomes_by_subject)[unique_subject_outcome]
      )
      subjects_by_sample <- lapply(split(belongs$to, belongs$from), unique)
      unique_subject <- lengths(subjects_by_sample) == 1L
      sample_ids_with_subject <- names(subjects_by_sample)[unique_subject]
      subject_of <- unlist(subjects_by_sample[unique_subject], use.names = FALSE)
      stratum[sample_ids_with_subject] <- unname(subject_key[subject_of])
    }
  }

  unname(stratum)
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
  if (identical(mode, "site")) return("leave_one_group_out")
  if (identical(mode, "region")) return("grouped_cv")
  if (identical(mode, "platform")) return("blocked_cv")
  if (identical(mode, "assay")) return("grouped_cv")
  if (identical(mode, "relatedness")) return("grouped_cv")
  if (identical(mode, "spatial")) return("grouped_cv")
  if (identical(mode, "time")) return("ordered_split")
  if (identical(mode, "composite") && identical(strategy, "strict")) return("custom_grouped_cv")
  if (identical(mode, "composite") && identical(strategy, "rule_based")) return("grouped_cv")
  "grouped_cv"
}

.split_spec_match_assignment_key <- function(assignments, sample_node_ids) {
  matched <- assignments[match(sample_node_ids, assignments$sample_node_id), , drop = FALSE]
  matched$linked_key
}

# Look up the direct assignment keys for one enrichment source. Enrichment is
# best-effort annotation, not the primary grouping, so an ambiguous assignment
# (e.g. a sample linked to two batches on a graph built with `validate =
# FALSE`) must not abort `as_split_spec()`. Such sources are left as NA and the
# reason is reported back so it can be recorded in the spec metadata.
.split_spec_enrichment_keys <- function(graph, mode, sample_ids) {
  tryCatch(
    list(
      keys = .split_spec_match_assignment_key(
        .depgraph_direct_assignment(graph, mode, samples = sample_ids),
        sample_ids
      ),
      warning = character()
    ),
    error = function(e) {
      list(
        keys = rep(NA_character_, length(sample_ids)),
        warning = paste0(
          "Could not enrich `", mode, "_group` from the graph; the column was ",
          "left as NA. Reason: ", conditionMessage(e)
        )
      )
    }
  )
}

.split_spec_enrichment_time <- function(graph, sample_ids) {
  tryCatch(
    {
      time_map <- .derive_time_constraints(graph, samples = sample_ids)$sample_map
      list(
        table = time_map[match(sample_ids, time_map$sample_node_id), , drop = FALSE],
        warning = character()
      )
    },
    error = function(e) {
      list(
        table = data.frame(
          timepoint_id = rep(NA_character_, length(sample_ids)),
          time_index = rep(NA_real_, length(sample_ids)),
          order_rank = rep(NA_integer_, length(sample_ids)),
          stringsAsFactors = FALSE
        ),
        warning = paste0(
          "Could not enrich time ordering (`timepoint_id`, `time_index`, ",
          "`order_rank`) from the graph; the columns were left as NA. Reason: ",
          conditionMessage(e)
        )
      )
    }
  )
}

.split_spec_fill_missing <- function(current, replacement) {
  missing <- is.na(current) | !nzchar(as.character(current))
  current[missing] <- replacement[missing]
  current
}

# Returns `sample_data` with the blocking / ordering annotation columns filled
# from the graph wherever the constraint left them NA. Any source that could
# not be resolved is reported through the "enrichment_warnings" attribute.
.split_spec_enrich_from_graph <- function(sample_data, graph) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  sample_ids <- sample_data$sample_node_id
  enrichment_warnings <- character()

  group_sources <- c(
    batch_group = "batch",
    study_group = "study",
    site_group = "site",
    region_group = "region",
    platform_group = "platform",
    assay_group = "assay"
  )
  for (column in names(group_sources)) {
    lookup <- .split_spec_enrichment_keys(graph, group_sources[[column]], sample_ids)
    sample_data[[column]] <- .split_spec_fill_missing(sample_data[[column]], lookup$keys)
    enrichment_warnings <- c(enrichment_warnings, lookup$warning)
  }

  time_lookup <- .split_spec_enrichment_time(graph, sample_ids)
  time_constraint <- time_lookup$table
  enrichment_warnings <- c(enrichment_warnings, time_lookup$warning)

  sample_data$stratum <- .split_spec_fill_missing(
    sample_data$stratum,
    .split_spec_stratum_from_graph(graph, sample_ids)
  )

  sample_data$timepoint_id <- .split_spec_fill_missing(sample_data$timepoint_id, time_constraint$timepoint_id)

  missing_time_index <- is.na(sample_data$time_index)
  sample_data$time_index[missing_time_index] <- time_constraint$time_index[missing_time_index]

  missing_order <- is.na(sample_data$order_rank)
  sample_data$order_rank[missing_order] <- time_constraint$order_rank[missing_order]

  attr(sample_data, "enrichment_warnings") <- enrichment_warnings
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
#' \code{primary_group}. When available, it also carries the blocking columns
#' (\code{batch_group}, \code{study_group}, \code{site_group},
#' \code{region_group}, \code{platform_group}, \code{assay_group}), the
#' \code{stratum} annotation, and the ordering columns (\code{timepoint_id},
#' \code{time_index}, \code{order_rank}). Missing but relevant fields are
#' retained as \code{NA} columns rather than omitted.
#'
#' \code{stratum} is filled from the graph when one is supplied: the key of the
#' single \code{Outcome} node attached to a sample via
#' \code{sample_has_outcome}, or, failing that, the single outcome attached to
#' the sample's subject via \code{subject_has_outcome}. It is an
#' \emph{annotation} of the outcome level each sample carries, exposed through
#' \code{stratum_var} so a downstream consumer (for example scikit-learn's
#' \code{StratifiedGroupKFold}) can stratify; splitGraph itself never balances
#' folds. When no sample has a unique outcome, \code{stratum_var} is
#' \code{NULL}.
#'
#' When only a subset of samples has ordering metadata, the translated split
#' spec still exposes that partial ordering through \code{time_var}, but
#' \code{ordering_required} remains \code{FALSE}. Ordering is only marked as
#' required when the constraint implies complete ordering coverage.
#'
#' When \code{graph} is supplied, the blocking and ordering annotation columns
#' are filled from the graph wherever the constraint left them \code{NA}. This
#' enrichment is best-effort: a source that cannot be resolved unambiguously
#' (for example a sample linked to two batches on a graph built with
#' \code{validate = FALSE}) is left as \code{NA} and the reason is recorded in
#' \code{metadata$enrichment_warnings} (and appended to
#' \code{metadata$warnings}) instead of aborting the translation. The primary
#' \code{group_id} always comes from the constraint and is never affected.
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
#' @section What downstream consumers read:
#' The \code{split_spec} contract is wider than any single consumer uses today.
#' Verified against the released versions on 2026-09-14:
#'
#' \tabular{lll}{
#'   \strong{Consumer} \tab \strong{Reads} \tab \strong{Modes} \cr
#'   bioLeak 0.3.8 \code{as_leaksplits()} \tab
#'     \code{sample_data} columns \code{sample_id}, the \code{group_var} column,
#'     \code{batch_group}, \code{study_group}, \code{timepoint_id},
#'     \code{order_rank}; fields \code{group_var}, \code{constraint_mode},
#'     \code{time_var} \tab
#'     subject, batch, study, time. Every other \code{constraint_mode}
#'     currently \emph{errors} inside bioLeak: site, region, platform, assay,
#'     relatedness and spatial are absent from its mode map ("subscript out of
#'     bounds"), and composite maps to \code{make_split_plan(mode =
#'     "combined")} without the \code{constraints} / \code{primary_axis} that
#'     mode requires. Until that is fixed, join \code{group_id} onto your
#'     observation frame and call \code{bioLeak::make_split_plan(group =
#'     "group_id")} directly; the grouping is preserved. \cr
#'   Python \code{splitspec} reader (shipped) \tab
#'     every field and column, including \code{stratum_var} / \code{stratum}
#'     and the block columns \tab all \cr
#'   rsample (adapter-cookbook vignette) \tab
#'     \code{group_id} for \code{group_vfold_cv()}, \code{order_rank} for
#'     \code{rolling_origin()}; block columns read for fold auditing \tab all \cr
#' }
#' Every row is pinned by a contract test (run when bioLeak is installed),
#' including the workaround, so the seam cannot drift silently; the test fails
#' deliberately when a bioLeak release starts accepting the other modes.
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
  if (identical(mode, "site")) {
    sample_data$site_group <- as.character(sample_map$group_label)
  }
  if (identical(mode, "region")) {
    sample_data$region_group <- as.character(sample_map$group_label)
  }
  if (identical(mode, "platform")) {
    sample_data$platform_group <- as.character(sample_map$group_label)
  }
  if (identical(mode, "assay")) {
    sample_data$assay_group <- as.character(sample_map$group_label)
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
  enrichment_warnings <- character()
  if (!is.null(graph)) {
    sample_data <- .split_spec_enrich_from_graph(sample_data, graph)
    enrichment_warnings <- attr(sample_data, "enrichment_warnings", exact = TRUE) %||% character()
    attr(sample_data, "enrichment_warnings") <- NULL
    enrichment_used <- TRUE
  }

  block_vars <- c()
  if (!all(is.na(sample_data$batch_group))) {
    block_vars <- c(block_vars, "batch_group")
  }
  if (!all(is.na(sample_data$study_group))) {
    block_vars <- c(block_vars, "study_group")
  }
  if (!all(is.na(sample_data$site_group))) {
    block_vars <- c(block_vars, "site_group")
  }
  if (!all(is.na(sample_data$region_group))) {
    block_vars <- c(block_vars, "region_group")
  }
  if (!all(is.na(sample_data$platform_group))) {
    block_vars <- c(block_vars, "platform_group")
  }
  if (!all(is.na(sample_data$assay_group))) {
    block_vars <- c(block_vars, "assay_group")
  }

  time_var <- if (!all(is.na(sample_data$order_rank))) "order_rank" else NULL
  stratum_var <- if (!all(is.na(sample_data$stratum))) "stratum" else NULL
  ordering_required <- isTRUE(constraint$recommended_downstream_args$ordering_required)

  split_spec(
    sample_data = sample_data,
    group_var = "group_id",
    block_vars = block_vars,
    time_var = time_var,
    stratum_var = stratum_var,
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
      via = as.character(constraint$metadata$via %||% character()),
      priority = as.character(constraint$metadata$priority %||% character()),
      threshold = constraint$metadata$threshold %||% NA_real_,
      threshold_metric = constraint$metadata$threshold_metric %||% NA_character_,
      splitgraph_version = .depgraph_package_version(),
      igraph_version = tryCatch(as.character(utils::packageVersion("igraph")), error = function(e) NA_character_),
      derived_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6%z"),
      n_samples = nrow(sample_data),
      n_groups = length(unique(sample_data$group_id)),
      warnings = c(constraint$metadata$warnings %||% character(), enrichment_warnings),
      enriched_from_graph = enrichment_used,
      enrichment_warnings = enrichment_warnings
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

  if (!is.null(x$stratum_var)) {
    if (!x$stratum_var %in% names(data)) {
      issues[[length(issues) + 1L]] <- .split_spec_new_issue(
        severity = "error",
        code = "invalid_stratum_var",
        message = paste0("Declared `stratum_var` is not present in `sample_data`: ", x$stratum_var)
      )
    } else {
      stratum_missing <- is.na(data[[x$stratum_var]]) | !nzchar(as.character(data[[x$stratum_var]]))
      if (all(stratum_missing)) {
        issues[[length(issues) + 1L]] <- .split_spec_new_issue(
          severity = "warning",
          code = "empty_stratum_var",
          message = paste0("Stratum variable `", x$stratum_var, "` is present but empty for all samples.")
        )
      } else if (any(stratum_missing)) {
        issues[[length(issues) + 1L]] <- .split_spec_new_issue(
          severity = "advisory",
          code = "partial_stratum",
          message = paste0(
            "Stratum variable `", x$stratum_var, "` is missing for ",
            sum(stratum_missing), " of ", nrow(data), " samples."
          ),
          n_affected = sum(stratum_missing)
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

.depgraph_constraint_via_modes <- function(constraint) {
  # Constraints store composite `via` as capitalized node types
  # (e.g., "Subject"); convert to lower-case mode names for severance lookup.
  via <- constraint$metadata$via %||% character()
  if (length(via) == 0L) return(character())
  reverse <- stats::setNames(
    names(.depgraph_constraint_mode_map),
    unname(.depgraph_constraint_mode_map)
  )
  matched <- reverse[as.character(via)]
  unname(matched[!is.na(matched)])
}

# Map a validation issue code to whether the chosen constraint mode (+ optional
# composite `via`) severs that leakage path. Returns TRUE/FALSE for codes the
# package can reason about, NA for codes whose severance is not a function of
# split-mode (structural errors, etc.) or when no constraint is supplied.
.leakage_severed_by_constraint <- function(code, mode = NULL, via_modes = character()) {
  if (is.null(mode) || !nzchar(mode)) return(NA)
  via_modes <- as.character(via_modes %||% character())
  composite_covers <- function(needed) {
    identical(mode, "composite") && needed %in% via_modes
  }
  switch(
    as.character(code),
    repeated_subject_samples     = identical(mode, "subject") || composite_covers("subject"),
    subject_cross_study_overlap  = mode %in% c("subject", "study") ||
                                     composite_covers("subject") ||
                                     composite_covers("study"),
    subject_cross_site_overlap   = mode %in% c("subject", "site") ||
                                     composite_covers("subject") ||
                                     composite_covers("site"),
    heavy_batch_reuse            = identical(mode, "batch") || composite_covers("batch"),
    missing_time_ordering        = identical(mode, "time") || composite_covers("time"),
    per_dataset_featureset       = FALSE,
    shared_featureset_provenance = FALSE,
    NA
  )
}

.leakage_summary_from_validation <- function(validation, constraint = NULL) {
  if (nrow(validation$issues) == 0L) {
    return(data.frame(
      severity = character(),
      category = character(),
      message = character(),
      source = character(),
      n_affected = integer(),
      severed = logical(),
      stringsAsFactors = FALSE
    ))
  }

  mode <- if (!is.null(constraint)) as.character(constraint$metadata$mode %||% NA) else NA_character_
  via_modes <- if (!is.null(constraint)) .depgraph_constraint_via_modes(constraint) else character()

  severed <- vapply(
    validation$issues$code,
    function(code) {
      .leakage_severed_by_constraint(code, mode = if (is.na(mode)) NULL else mode, via_modes = via_modes)
    },
    logical(1)
  )

  data.frame(
    severity = validation$issues$severity,
    category = validation$issues$code,
    message = validation$issues$message,
    source = "validation",
    n_affected = vapply(validation$issues$node_ids, length, integer(1)),
    severed = severed,
    stringsAsFactors = FALSE
  )
}

.leakage_summary_from_constraint <- function(constraint) {
  empty <- data.frame(
    severity = character(),
    category = character(),
    message = character(),
    source = character(),
    n_affected = integer(),
    severed = logical(),
    stringsAsFactors = FALSE
  )
  if (is.null(constraint)) {
    return(empty)
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
        severed = NA,
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
        severed = NA,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(diagnostics) == 0L) {
    return(empty)
  }

  out <- do.call(rbind, diagnostics)
  row.names(out) <- NULL
  out
}

.leakage_summary_from_split_spec <- function(split_spec) {
  empty <- data.frame(
    severity = character(),
    category = character(),
    message = character(),
    source = character(),
    n_affected = integer(),
    severed = logical(),
    stringsAsFactors = FALSE
  )
  if (is.null(split_spec)) {
    return(list(diagnostics = empty, summary = list()))
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
      severed = NA,
      stringsAsFactors = FALSE
    )
  } else {
    diagnostics[[length(diagnostics) + 1L]] <- data.frame(
      severity = validation$issues$severity,
      category = validation$issues$code,
      message = validation$issues$message,
      source = "split_spec",
      n_affected = validation$issues$n_affected,
      severed = NA,
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
      severed = NA,
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
        severed = NA,
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
      severed = NA,
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
  validation_diag <- .leakage_summary_from_validation(validation, constraint = constraint)
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
      severed = logical(),
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

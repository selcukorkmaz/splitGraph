# Print / summary / as.data.frame methods and rarely reached validation
# branches. These exist mainly to keep the covr gate honest: every S3 method
# has to at least run and return its object, and every validation code the
# package documents has to be reachable by a test.

cov_graph <- function() {
  meta <- data.frame(
    sample_id    = c("S1", "S2", "S3", "S4"),
    subject_id   = c("P1", "P1", "P2", "P2"),
    batch_id     = c("B1", "B2", "B1", "B2"),
    study_id     = c("ST1", "ST1", "ST2", "ST2"),
    timepoint_id = c("T1", "T2", "T1", "T2"),
    time_index   = c(1, 2, 1, 2),
    outcome_id   = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "cov")
}

test_that("every S3 method prints, summarises, and converts", {
  g <- cov_graph()
  con <- derive_split_constraints(g, "time")
  spec <- as_split_spec(con, graph = g)
  report <- validate_graph(g)
  preflight <- validate_split_spec(spec)
  risks <- summarize_leakage_risks(g, constraint = con, split_spec = spec)
  q <- query_node_type(g, "Sample")

  objects <- list(g$nodes, g$edges, g, q, con, report, spec, preflight, risks)
  for (obj in objects) {
    expect_output(print(obj))
    expect_identical(print(obj), obj)
    s <- summary(obj)
    expect_true(is.list(s))
  }
  expect_s3_class(as.data.frame(g$nodes), "data.frame")
  expect_s3_class(as.data.frame(g$edges), "data.frame")
  expect_s3_class(as.data.frame(q), "data.frame")
  expect_s3_class(as.data.frame(con), "data.frame")
  expect_s3_class(as.data.frame(report), "data.frame")
  expect_s3_class(as.data.frame(spec), "data.frame")
  expect_s3_class(as.data.frame(preflight), "data.frame")
  expect_s3_class(as.data.frame(risks), "data.frame")

  # summary details that carry information
  expect_identical(summary(spec)$stratum_var, "stratum")  # outcomes present -> stratum enriched from graph
  expect_output(print(spec), "Stratum var: stratum")
  expect_identical(summary(con)$mode, "time")
  expect_true(all(c("n_nodes", "n_edges", "node_types", "edge_types") %in% names(summary(g))))
  expect_identical(summary(report)$n_issues, nrow(report$issues))
  expect_true("by_source" %in% names(summary(risks)))
})

test_that("print methods handle unnamed graphs, empty specs, and JSON reports", {
  meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"), stringsAsFactors = FALSE)
  g <- graph_from_metadata(meta)
  expect_output(print(g), "<unnamed>")
  expect_output(print(split_spec()), "<split_spec>")
  expect_output(print(validate_split_spec(split_spec())), "Valid")
  expect_output(print(leakage_risk_summary()), "Diagnostics")

  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  expect_output(print(validate_graph_json(tmp)), "valid:   TRUE")
  writeLines('{"splitGraph_object": "split_spec", "schema_version": "x"}', tmp)
  expect_output(print(validate_split_spec_json(tmp)), "issues:")
})

test_that("plot variants render without error", {
  g <- cov_graph()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(g, layout = "auto", legend = FALSE))
  expect_silent(plot(g, layout = igraph::layout_in_circle(as_igraph(g))))
  expect_silent(plot(g, layout = function(x) igraph::layout_with_fr(x)))
  expect_silent(plot(g, node_colors = c(Sample = "red"), vertex.size = 5, legend_position = "bottomright"))
})

test_that("rare structural and semantic validation codes are reachable", {
  # Hand-built tables reach codes the constructors normally prevent.
  nodes <- data.frame(
    node_id = c("sample:S1", "sample:S2", "timepoint:T1", "timepoint:T2", "featureset:F1", "outcome:O1", "assay:A1", "assay:A2"),
    node_type = c("Sample", "Sample", "Timepoint", "Timepoint", "FeatureSet", "Outcome", "Assay", "Assay"),
    node_key = c("S1", "S2", "T1", "T2", "F1", "O1", "A1", "A2"),
    label = c("S1", "S2", "T1", "T2", "F1", "O1", "A1", "A2"),
    attrs = I(list(
      list(), list(),
      list(time_index = "two"), list(time_index = 1),
      list(derivation_scope = "everywhere"),
      list(observation_level = "cohort"),
      list(), list()
    )),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    edge_id = c("e1", "e2", "e3", "e4", "e5", "e6", "e7"),
    from = c("sample:S1", "sample:S1", "timepoint:T1", "timepoint:T1", "timepoint:T2", "sample:S1", "sample:S1"),
    to = c("assay:A1", "assay:A2", "timepoint:T1", "timepoint:T2", "timepoint:T1", "featureset:F1", "outcome:O1"),
    edge_type = c("sample_measured_by_assay", "sample_measured_by_assay", "timepoint_precedes",
                  "timepoint_precedes", "timepoint_precedes", "sample_uses_featureset", "sample_has_outcome"),
    attrs = I(rep(list(list()), 7)),
    stringsAsFactors = FALSE
  )
  g <- dependency_graph(graph_node_set(nodes), graph_edge_set(edges), graph = NULL)
  report <- validate_graph(g)
  codes <- report$issues$code
  expect_true("single_target_violation" %in% codes)          # S1 measured by two assays
  expect_true("timepoint_self_loop" %in% codes)              # T1 -> T1
  expect_true("timepoint_precedence_cycle" %in% codes)       # T1 -> T2 -> T1
  expect_true("invalid_time_index" %in% codes)               # "two"
  expect_true("invalid_featureset_derivation_scope" %in% codes)
  expect_true("invalid_outcome_observation_level" %in% codes)
  expect_false(report$valid)

  # Filtering by severity keeps validity based on all issues.
  filtered <- validate_graph(g, severities = "advisory")
  expect_false(filtered$valid)
  expect_true(all(filtered$issues$severity == "advisory"))
  expect_error(validate_graph(g, levels = "nonsense"), "unsupported")
  expect_error(validate_graph(g, severities = "loud"), "unsupported")
})

test_that("unknown edge types and dangling endpoints are structural errors", {
  nodes <- data.frame(
    node_id = c("sample:S1", "subject:P1"), node_type = c("Sample", "Subject"),
    node_key = c("S1", "P1"), label = c("S1", "P1"), attrs = I(list(list(), list())),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    edge_id = c("x1", "x2"), from = c("sample:S1", "sample:S1"), to = c("subject:P1", "subject:P1"),
    edge_type = c("sample_is_friends_with", "sample_belongs_to_subject"),
    attrs = I(list(list(), list())), stringsAsFactors = FALSE
  )
  g <- dependency_graph(graph_node_set(nodes), graph_edge_set(edges), graph = NULL)
  report <- validate_graph(g)
  expect_true("unsupported_edge_type" %in% report$issues$code)
  expect_false(report$valid)
})

test_that("time ordering falls back to precedence edges and reports partial coverage", {
  meta <- data.frame(
    sample_id = c("S1", "S2", "S3"), timepoint_id = c("T1", "T2", "T3"), stringsAsFactors = FALSE
  )
  samples <- create_nodes(meta, "Sample", "sample_id")
  tps <- create_nodes(meta, "Timepoint", "timepoint_id")
  e1 <- create_edges(meta, "sample_id", "timepoint_id", "Sample", "Timepoint", "sample_collected_at_timepoint")
  prec <- create_edges(data.frame(a = c("T1", "T2"), b = c("T2", "T3")), "a", "b", "Timepoint", "Timepoint", "timepoint_precedes")
  g <- build_dependency_graph(list(samples, tps), list(e1, prec))
  con <- derive_split_constraints(g, "time")
  expect_identical(con$metadata$time_order_source, "timepoint_precedes")
  expect_identical(con$sample_map$order_rank, 1:3)
  expect_true("timepoint_precedes" %in% con$metadata$relations_used)

  # No ordering information at all: warnings, NA ranks, ordering not required.
  g2 <- build_dependency_graph(list(samples, tps), list(e1))
  con2 <- derive_split_constraints(g2, "time")
  expect_true(all(is.na(con2$sample_map$order_rank)))
  expect_false(con2$recommended_downstream_args$ordering_required)
  expect_true(any(grepl("unavailable", con2$metadata$warnings)))
  expect_identical(derive_split_constraints(g2, "time", include_warnings = FALSE)$metadata$warnings, character())
})

test_that("query helpers cover edge filters, paths, and truncation flags", {
  g <- cov_graph()
  n_out <- query_neighbors(g, "sample:S1", direction = "out", node_types = "Subject")
  expect_true(all(as.data.frame(n_out)$node_type == "Subject"))
  n_in <- query_neighbors(g, "subject:P1", direction = "in")
  expect_true(all(as.data.frame(n_in)$node_type == "Sample"))
  n_all <- query_neighbors(g, "subject:P1", direction = "all", edge_types = "sample_belongs_to_subject")
  expect_identical(nrow(as.data.frame(n_all)), 2L)

  e <- query_edge_type(g, "sample_processed_in_batch", node_ids = "sample:S1")
  expect_identical(nrow(as.data.frame(e)), 1L)

  p <- query_paths(g, "sample:S1", "outcome:a", mode = "out", node_types = c("Sample", "Outcome"))
  expect_true(nrow(as.data.frame(p)) >= 2L)
  expect_false(p$metadata$truncated)
  p_all <- query_paths(g, "sample:S1", "sample:S2", mode = "all", max_length = 2)
  expect_true(is.logical(p_all$metadata$truncated))
  sp <- query_shortest_paths(g, "sample:S1", "sample:S2", mode = "all", node_types = c("Sample", "Subject"))
  expect_identical(unique(as.data.frame(sp)$path_id), "path_1")
  # igraph warns that S3 is unreachable in "out" mode; that is the case under test.
  none <- suppressWarnings(query_shortest_paths(g, "sample:S1", "sample:S3", mode = "out"))
  expect_identical(nrow(as.data.frame(none)), 0L)
  expect_error(query_paths(g, "sample:S1", "sample:S2", max_length = -1), "non-negative")
  expect_error(query_paths(g, "sample:S1", "sample:S2", max_length = "a"), "numeric")
})

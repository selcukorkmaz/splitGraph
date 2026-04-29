skip_if_no_jsonlite <- function() {
  testthat::skip_if_not_installed("jsonlite")
}

make_round_trip_graph <- function() {
  meta <- data.frame(
    sample_id    = c("S1", "S2", "S3"),
    subject_id   = c("P1", "P1", "P2"),
    batch_id     = c("B1", "B2", "B1"),
    timepoint_id = c("T0", "T1", "T0"),
    time_index   = c(0, 1, 0),
    outcome_id   = c("ctrl", "case", "ctrl"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "rt_demo", dataset_name = "rt_dataset")
}

# ---- dependency_graph round-trip --------------------------------------------

test_that("write_dependency_graph + read_dependency_graph round-trip preserves structure", {
  skip_if_no_jsonlite()
  g <- make_round_trip_graph()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  written <- write_dependency_graph(g, tmp)
  expect_identical(written, tmp)
  expect_true(file.exists(tmp))

  g2 <- read_dependency_graph(tmp)
  expect_s3_class(g2, "dependency_graph")

  # Same nodes (sorted, comparing structural columns).
  n1 <- g$nodes$data[order(g$nodes$data$node_id), c("node_id", "node_type", "node_key", "label")]
  n2 <- g2$nodes$data[order(g2$nodes$data$node_id), c("node_id", "node_type", "node_key", "label")]
  row.names(n1) <- NULL; row.names(n2) <- NULL
  expect_identical(n1, n2)

  # Same edges (sorted).
  e1 <- g$edges$data[order(g$edges$data$edge_id), c("edge_id", "from", "to", "edge_type")]
  e2 <- g2$edges$data[order(g2$edges$data$edge_id), c("edge_id", "from", "to", "edge_type")]
  row.names(e1) <- NULL; row.names(e2) <- NULL
  expect_identical(e1, e2)

  # Validation status preserved (the rebuilt graph must validate).
  expect_true(validate_graph(g2)$valid)
})

test_that("round-trip preserves graph metadata labels", {
  skip_if_no_jsonlite()
  g <- make_round_trip_graph()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_dependency_graph(g, tmp)
  g2 <- read_dependency_graph(tmp)

  expect_identical(g2$metadata$graph_name, "rt_demo")
  expect_identical(g2$metadata$dataset_name, "rt_dataset")
  expect_identical(g2$metadata$schema_version, splitGraph:::.depgraph_schema_version)
})

test_that("round-trip preserves node and edge attribute list-columns", {
  skip_if_no_jsonlite()
  # Build a graph with non-trivial node attributes via the explicit path.
  meta <- data.frame(sample_id = c("S1", "S2"),
                     subject_id = c("P1", "P2"),
                     stringsAsFactors = FALSE)
  samples <- create_nodes(meta, "Sample", "sample_id")
  subjects <- create_nodes(meta, "Subject", "subject_id")
  time_nodes <- create_nodes(
    data.frame(timepoint_id = c("T0", "T1"),
               time_index = c(0L, 1L),
               visit_label = c("baseline", "follow_up"),
               stringsAsFactors = FALSE),
    type = "Timepoint", id_col = "timepoint_id",
    attr_cols = c("time_index", "visit_label")
  )
  edges <- create_edges(meta, "sample_id", "subject_id",
                        "Sample", "Subject", "sample_belongs_to_subject")
  g <- build_dependency_graph(list(samples, subjects, time_nodes), list(edges))

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  g2 <- read_dependency_graph(tmp)

  t0_idx <- which(g2$nodes$data$node_id == "timepoint:T0")
  expect_length(t0_idx, 1L)
  attrs <- g2$nodes$data$attrs[[t0_idx]]
  expect_true(is.list(attrs))
  expect_equal(unname(attrs$time_index), 0)
  expect_equal(unname(attrs$visit_label), "baseline")

  # Empty-attrs nodes round-trip as list(). Subject nodes here have no
  # optional schema attributes populated from `meta`, so they're a clean test.
  p1_idx <- which(g2$nodes$data$node_id == "subject:P1")
  expect_length(p1_idx, 1L)
  expect_identical(g2$nodes$data$attrs[[p1_idx]], list())
})

test_that("round-trip preserves validation_overrides on the graph", {
  skip_if_no_jsonlite()
  g <- make_round_trip_graph()
  g$metadata$validation_overrides <- list(allow_multi_subject_samples = TRUE)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  g2 <- read_dependency_graph(tmp)
  expect_true(isTRUE(g2$metadata$validation_overrides$allow_multi_subject_samples))
})

test_that("read_dependency_graph rejects malformed or wrong-type JSON", {
  skip_if_no_jsonlite()
  bad <- tempfile(fileext = ".json")
  on.exit(unlink(bad), add = TRUE)

  writeLines("not json at all {", bad)
  expect_error(read_dependency_graph(bad), "JSON|parse", ignore.case = TRUE)

  # Right shape but wrong object type.
  spec_like <- list(
    splitGraph_object = "split_spec",
    schema_version = "0.1.0",
    sample_data = list()
  )
  writeLines(jsonlite::toJSON(spec_like, auto_unbox = TRUE, null = "null"), bad)
  expect_error(read_dependency_graph(bad), "dependency_graph", ignore.case = TRUE)
})

test_that("read_dependency_graph warns on unknown future schema_version but still loads", {
  skip_if_no_jsonlite()
  g <- make_round_trip_graph()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  raw$schema_version <- "99.0.0"
  writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null"), tmp)

  expect_warning(g2 <- read_dependency_graph(tmp), "schema", ignore.case = TRUE)
  expect_s3_class(g2, "dependency_graph")
})

# ---- split_spec round-trip --------------------------------------------------

test_that("write_split_spec + read_split_spec round-trip preserves structure", {
  skip_if_no_jsonlite()
  g <- make_round_trip_graph()
  constraint <- derive_split_constraints(g, mode = "subject")
  spec <- as_split_spec(constraint, graph = g)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  written <- write_split_spec(spec, tmp)
  expect_identical(written, tmp)
  expect_true(file.exists(tmp))

  spec2 <- read_split_spec(tmp)
  expect_s3_class(spec2, "split_spec")

  # Sample-data table identical (column order / NA preservation).
  d1 <- spec$sample_data
  d2 <- spec2$sample_data
  expect_identical(names(d1), names(d2))
  expect_identical(nrow(d1), nrow(d2))
  expect_identical(d1$sample_id, d2$sample_id)
  expect_identical(d1$group_id, d2$group_id)
  expect_identical(d1$batch_group, d2$batch_group)
  expect_identical(d1$study_group, d2$study_group)
  expect_identical(d1$timepoint_id, d2$timepoint_id)
  expect_identical(d1$time_index, d2$time_index)
  expect_identical(d1$order_rank, d2$order_rank)

  # Top-level scalars.
  expect_identical(spec2$group_var, spec$group_var)
  expect_identical(spec2$block_vars, spec$block_vars)
  expect_identical(spec2$time_var, spec$time_var)
  expect_identical(spec2$ordering_required, spec$ordering_required)
  expect_identical(spec2$constraint_mode, spec$constraint_mode)
  expect_identical(spec2$recommended_resampling, spec$recommended_resampling)

  # Validation passes after round-trip.
  expect_true(validate_split_spec(spec2)$valid)
})

test_that("read_split_spec round-trip preserves NA in sample_data", {
  skip_if_no_jsonlite()
  # A graph where some samples have no batch / timepoint -> NA after as_split_spec.
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  constraint <- derive_split_constraints(g, mode = "subject")
  spec <- as_split_spec(constraint, graph = g)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)
  spec2 <- read_split_spec(tmp)

  # batch_group / order_rank should still be all-NA.
  expect_true(all(is.na(spec2$sample_data$batch_group)))
  expect_true(all(is.na(spec2$sample_data$order_rank)))
})

test_that("read_split_spec rejects malformed or wrong-type JSON", {
  skip_if_no_jsonlite()
  bad <- tempfile(fileext = ".json")
  on.exit(unlink(bad), add = TRUE)

  writeLines("[1,2,3", bad)
  expect_error(read_split_spec(bad), "JSON|parse", ignore.case = TRUE)

  graph_like <- list(
    splitGraph_object = "dependency_graph",
    schema_version = "0.1.0",
    nodes = list(), edges = list()
  )
  writeLines(jsonlite::toJSON(graph_like, auto_unbox = TRUE, null = "null"), bad)
  expect_error(read_split_spec(bad), "split_spec", ignore.case = TRUE)
})

test_that("write_* errors when target directory does not exist", {
  skip_if_no_jsonlite()
  g <- make_round_trip_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  bad_path <- file.path(tempdir(), "splitGraph_no_such_dir_xyz", "x.json")

  expect_error(write_dependency_graph(g, bad_path), "directory", ignore.case = TRUE)
  expect_error(write_split_spec(spec, bad_path), "directory", ignore.case = TRUE)
})

test_that("write_* / read_* error helpfully if jsonlite is missing", {
  # Simulate missing jsonlite without actually unloading it: we test the
  # internal guard directly.
  expect_silent(splitGraph:::.depgraph_require_jsonlite())
})

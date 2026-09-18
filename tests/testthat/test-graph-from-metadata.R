test_that("graph_from_metadata auto-detects canonical columns", {
  meta <- data.frame(
    sample_id    = c("S1", "S2", "S3", "S4"),
    subject_id   = c("P1", "P1", "P2", "P2"),
    batch_id     = c("B1", "B2", "B1", "B2"),
    timepoint_id = c("T1", "T2", "T1", "T2"),
    time_index   = c(1, 2, 1, 2),
    outcome_id   = c("ctrl", "case", "ctrl", "case"),
    stringsAsFactors = FALSE
  )

  g <- graph_from_metadata(meta, graph_name = "demo")

  expect_s3_class(g, "dependency_graph")
  node_types <- sort(unique(g$nodes$data$node_type))
  expect_true(all(c("Sample", "Subject", "Batch", "Timepoint", "Outcome") %in% node_types))

  edge_types <- sort(unique(g$edges$data$edge_type))
  expect_true("sample_belongs_to_subject" %in% edge_types)
  expect_true("sample_processed_in_batch" %in% edge_types)
  expect_true("sample_collected_at_timepoint" %in% edge_types)
  expect_true("sample_has_outcome" %in% edge_types)
  expect_true("timepoint_precedes" %in% edge_types)
})

test_that("graph_from_metadata skips absent columns silently", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  g <- graph_from_metadata(meta)
  expect_s3_class(g, "dependency_graph")
  expect_false("Batch" %in% g$nodes$data$node_type)
  expect_false("Timepoint" %in% g$nodes$data$node_type)
})

test_that("graph_from_metadata supports subject-scope outcome", {
  meta <- data.frame(
    sample_id     = c("S1", "S2", "S3"),
    subject_id    = c("P1", "P2", "P3"),
    outcome_id    = c("ctrl", "case", "ctrl"),
    stringsAsFactors = FALSE
  )

  g <- graph_from_metadata(meta, outcome_scope = "subject")
  edge_types <- unique(g$edges$data$edge_type)
  expect_true("subject_has_outcome" %in% edge_types)
  expect_false("sample_has_outcome" %in% edge_types)
})

test_that("graph_from_metadata accepts factor and numeric site/region/platform ids", {
  meta <- data.frame(
    sample_id   = c("S1", "S2", "S3"),
    subject_id  = c("P1", "P2", "P3"),
    site_id     = factor(c("A", "B", "A")),
    region_id   = factor(c("cortex", "cortex", "liver")),
    platform_id = c(1, 2, 1),
    stringsAsFactors = FALSE
  )

  ingested <- ingest_metadata(meta)
  expect_type(ingested$site_id, "character")
  expect_type(ingested$region_id, "character")
  expect_type(ingested$platform_id, "character")

  g <- graph_from_metadata(meta)
  expect_s3_class(g, "dependency_graph")
  expect_setequal(
    g$nodes$data$node_key[g$nodes$data$node_type == "Site"],
    c("A", "B")
  )
  expect_setequal(
    g$nodes$data$node_key[g$nodes$data$node_type == "Platform"],
    c("1", "2")
  )
  expect_identical(
    unname(grouping_vector(derive_split_constraints(g, mode = "site"))),
    c("site:A", "site:B", "site:A")
  )
})

test_that("create_nodes accepts a factor identifier column directly", {
  nodes <- create_nodes(data.frame(site_id = factor(c("A", "B", "A"))), "Site", "site_id")
  expect_identical(nodes$data$node_key, c("A", "B"))
})

test_that("a metadata table with only sample_id yields an edgeless graph", {
  # `sample_id` is documented as the only required column, so this must build
  # rather than fail in the edge binder with an internal message.
  g <- graph_from_metadata(data.frame(sample_id = c("S1", "S2"), stringsAsFactors = FALSE))
  expect_s3_class(g, "dependency_graph")
  expect_identical(nrow(g$nodes$data), 2L)
  expect_identical(nrow(g$edges$data), 0L)
  expect_true(validate_graph(g)$valid)

  # the rest of the pipeline still works on it
  con <- derive_split_constraints(g, "subject")
  expect_length(grouping_vector(con), 2L)
  spec <- as_split_spec(con, graph = g)
  expect_s3_class(spec, "split_spec")

  # columns that are not canonical are ignored, not fatal
  expect_s3_class(
    graph_from_metadata(data.frame(sample_id = c("S1", "S2"), extra = c(1, 2))),
    "dependency_graph"
  )

  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  expect_true(validate_graph_json(tmp)$valid)
  expect_identical(nrow(read_dependency_graph(tmp, validate = TRUE)$edges$data), 0L)
})

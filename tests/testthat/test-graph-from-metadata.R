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

test_that("graph_from_metadata warns when outcome_value is numeric", {
  meta <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    subject_id = c("P1", "P2", "P3"),
    outcome_value = c(0, 1, 0),
    stringsAsFactors = FALSE
  )

  expect_warning(
    g <- graph_from_metadata(meta),
    "outcome_value",
    ignore.case = TRUE
  )
  expect_s3_class(g, "dependency_graph")
  # The Outcome nodes are still constructed (compatibility) but with numeric-derived keys.
  outcome_keys <- g$nodes$data$node_key[g$nodes$data$node_type == "Outcome"]
  expect_setequal(outcome_keys, c("0", "1"))
})

test_that("graph_from_metadata is silent when outcome_id is character", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    outcome_id = c("case", "ctrl"),
    stringsAsFactors = FALSE
  )

  expect_silent(g <- graph_from_metadata(meta))
  outcome_keys <- g$nodes$data$node_key[g$nodes$data$node_type == "Outcome"]
  expect_setequal(outcome_keys, c("case", "ctrl"))
})

test_that("graph_from_metadata is silent when outcome_value is character (case/ctrl)", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    outcome_value = c("case", "ctrl"),
    stringsAsFactors = FALSE
  )
  expect_silent(g <- graph_from_metadata(meta))
})

test_that("graph_from_metadata is silent when no outcome columns are present", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  expect_silent(graph_from_metadata(meta))
})

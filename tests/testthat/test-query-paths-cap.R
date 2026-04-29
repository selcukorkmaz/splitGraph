make_path_graph <- function() {
  meta <- data.frame(
    sample_id    = c("S1", "S2", "S3"),
    subject_id   = c("P1", "P2", "P3"),
    timepoint_id = c("T0", "T1", "T2"),
    time_index   = c(0, 1, 2),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "path_demo")
}

test_that("query_paths uses a finite default safety cap and returns finite results", {
  g <- make_path_graph()
  # The default max_length should be finite — no Inf, no -1.
  res <- query_paths(g, from = "sample:S1", to = "subject:P1")
  expect_s3_class(res, "graph_query_result")
  # We should reach P1 from S1 within the cap.
  expect_true(nrow(res$table) >= 1L)
})

test_that("query_paths(max_length = NULL) is equivalent to the documented default cap", {
  g <- make_path_graph()
  default_res <- query_paths(g, from = "sample:S1", to = "subject:P1")
  explicit_res <- query_paths(
    g,
    from = "sample:S1",
    to = "subject:P1",
    max_length = splitGraph:::.depgraph_default_path_cap
  )
  expect_identical(default_res$table, explicit_res$table)
})

test_that("query_paths(max_length = Inf) opts out of the cap", {
  g <- make_path_graph()
  uncapped <- query_paths(
    g,
    from = "sample:S1",
    to = "subject:P1",
    max_length = Inf
  )
  expect_s3_class(uncapped, "graph_query_result")
  expect_true(nrow(uncapped$table) >= 1L)
})

test_that("query_paths rejects negative or non-numeric max_length", {
  g <- make_path_graph()
  expect_error(
    query_paths(g, from = "sample:S1", to = "subject:P1", max_length = -2),
    "max_length"
  )
  expect_error(
    query_paths(g, from = "sample:S1", to = "subject:P1", max_length = "five"),
    "max_length"
  )
})

test_that("query_paths(max_length = 0) returns no paths (boundary)", {
  g <- make_path_graph()
  zero <- query_paths(g, from = "sample:S1", to = "subject:P1", max_length = 0)
  # cutoff = 0 means no edges -> no paths between distinct nodes.
  expect_s3_class(zero, "graph_query_result")
  expect_equal(nrow(zero$table), 0L)
})

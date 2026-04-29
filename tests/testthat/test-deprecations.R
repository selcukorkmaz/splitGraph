make_simple_graph <- function() {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject"
  )
  build_dependency_graph(list(samples, subjects), list(edges))
}

test_that("validate_graph(checks = ...) is deprecated but still works", {
  g <- make_simple_graph()

  # Default call (no `checks` supplied) must NOT warn.
  expect_silent(validate_graph(g))

  # Explicit `checks` triggers a deprecation warning.
  expect_warning(
    report <- validate_graph(g, checks = c("ids", "references")),
    "deprecated",
    ignore.case = TRUE
  )
  expect_s3_class(report, "depgraph_validation_report")
  expect_true(report$valid)
})

test_that("validate_graph() recommended path (levels=) is silent and equivalent", {
  g <- make_simple_graph()

  expect_silent(report <- validate_graph(g, levels = c("structural", "semantic")))
  expect_s3_class(report, "depgraph_validation_report")
  expect_true(report$valid)
})

test_that("alias functions are deprecated but still functional", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject"
  )

  expect_warning(
    g <- build_depgraph(list(samples, subjects), list(edges)),
    "deprecated", ignore.case = TRUE
  )
  expect_s3_class(g, "dependency_graph")

  expect_warning(
    report <- validate_depgraph(g),
    "deprecated", ignore.case = TRUE
  )
  expect_s3_class(report, "depgraph_validation_report")

  # Constructor aliases.
  expect_warning(
    n <- new_depgraph_nodes(),
    "deprecated", ignore.case = TRUE
  )
  expect_s3_class(n, "graph_node_set")

  expect_warning(
    e <- new_depgraph_edges(),
    "deprecated", ignore.case = TRUE
  )
  expect_s3_class(e, "graph_edge_set")

  # new_depgraph wraps dependency_graph(), which expects already-bound
  # node/edge sets — we mimic build_dependency_graph()'s assembly:
  bound_nodes <- graph_node_set(rbind(
    as.data.frame(create_nodes(meta, "Sample", "sample_id")),
    as.data.frame(create_nodes(meta, "Subject", "subject_id"))
  ))
  bound_edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject"
  )
  expect_warning(
    nd <- new_depgraph(nodes = bound_nodes, edges = bound_edges, graph = NULL),
    "deprecated", ignore.case = TRUE
  )
  expect_s3_class(nd, "dependency_graph")
})

test_that("the canonical (non-deprecated) constructors produce no warnings", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  expect_silent(graph_node_set(data.frame(
    node_id = c("sample:S1"), node_type = "Sample",
    node_key = "S1", label = "S1",
    attrs = I(list(list())), stringsAsFactors = FALSE
  )))
  expect_silent(graph_edge_set())

  samples <- create_nodes(meta, "Sample", "sample_id")
  subjects <- create_nodes(meta, "Subject", "subject_id")
  edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject"
  )
  expect_silent(build_dependency_graph(list(samples, subjects), list(edges)))
})

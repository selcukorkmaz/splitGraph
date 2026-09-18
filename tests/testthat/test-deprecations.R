# The 0.1-era aliases were deprecated in 0.2.0 and removed in 0.4.0. These
# tests pin the removal so the names cannot silently come back, and check the
# canonical replacements work without warnings.

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

test_that("the removed 0.1-era aliases are no longer exported", {
  removed <- c(
    "new_depgraph", "new_depgraph_nodes", "new_depgraph_edges",
    "build_depgraph", "validate_depgraph"
  )
  exported <- getNamespaceExports("splitGraph")
  expect_false(any(removed %in% exported))
  for (name in removed) {
    expect_false(exists(name, envir = asNamespace("splitGraph"), inherits = FALSE), info = name)
  }
})

test_that("validate_graph() no longer accepts the removed `checks` argument", {
  g <- make_simple_graph()
  expect_error(validate_graph(g, checks = c("ids", "references")), "unused argument")
})

test_that("validate_graph() recommended path (levels=) is silent", {
  g <- make_simple_graph()

  expect_silent(report <- validate_graph(g, levels = c("structural", "semantic")))
  expect_s3_class(report, "depgraph_validation_report")
  expect_true(report$valid)
  expect_identical(report$metadata$levels, c("structural", "semantic"))
})

test_that("the canonical constructors produce no warnings", {
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
  expect_silent(dependency_graph(
    nodes = graph_node_set(rbind(samples$data, subjects$data)),
    edges = edges,
    graph = NULL
  ))
})

export_graph_fixture <- function() {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    batch_id   = c("B1", "B2", "B1"),
    sample_role = c("case", "control", "case"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "export-demo")
}

test_that("export_graph writes GraphML that igraph reads back with attributes", {
  g <- export_graph_fixture()
  tmp <- tempfile(fileext = ".graphml")
  on.exit(unlink(tmp), add = TRUE)
  out <- export_graph(g, tmp, format = "graphml")
  expect_true(file.exists(tmp))
  expect_type(out, "character")

  back <- igraph::read_graph(tmp, format = "graphml")
  expect_equal(igraph::vcount(back), nrow(g$nodes$data))
  expect_equal(igraph::ecount(back), nrow(g$edges$data))
  expect_true("node_type" %in% igraph::vertex_attr_names(back))
  expect_true("attr_sample_role" %in% igraph::vertex_attr_names(back))
  expect_true("edge_type" %in% igraph::edge_attr_names(back))
  roles <- igraph::vertex_attr(back, "attr_sample_role")
  expect_setequal(roles[igraph::vertex_attr(back, "node_type") == "Sample"], c("case", "control", "case"))
})

test_that("export_graph writes GML and CSV tables", {
  g <- export_graph_fixture()
  gml <- tempfile(fileext = ".gml")
  nodes_csv <- tempfile(fileext = ".csv")
  edges_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(gml, nodes_csv, edges_csv)), add = TRUE)

  export_graph(g, gml, format = "gml")
  expect_equal(igraph::vcount(igraph::read_graph(gml, format = "gml")), nrow(g$nodes$data))

  export_graph(g, nodes_csv, format = "nodes_csv")
  nodes <- utils::read.csv(nodes_csv, stringsAsFactors = FALSE)
  expect_identical(nrow(nodes), nrow(g$nodes$data))
  expect_true(all(c("node_id", "node_type", "node_key", "label", "attr_sample_role") %in% names(nodes)))

  export_graph(g, edges_csv, format = "edges_csv")
  edges <- utils::read.csv(edges_csv, stringsAsFactors = FALSE)
  expect_identical(nrow(edges), nrow(g$edges$data))
  expect_true(all(c("from", "to", "edge_id", "edge_type") %in% names(edges)))
})

test_that("attribute flattening keeps numeric columns numeric and collapses vectors", {
  attrs <- list(list(a = 1, b = "x"), list(a = 2.5), list(b = c("y", "z"), c = TRUE))
  flat <- splitGraph:::.depgraph_flatten_attrs(attrs)
  expect_identical(names(flat), c("attr_a", "attr_b", "attr_c"))
  expect_type(flat$attr_a, "double")
  expect_identical(flat$attr_b, c("x", NA, "y;z"))
  expect_identical(flat$attr_c, c(NA, NA, TRUE))
})

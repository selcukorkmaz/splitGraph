make_region_graph <- function() {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P1", "P2", "P2"),
    region_id  = c("cortex", "cortex", "hippocampus", "hippocampus"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "region-demo")
}

test_that("graph_from_metadata auto-detects region_id and builds Region nodes and edges", {
  g <- make_region_graph()
  nd <- g$nodes$data
  expect_equal(sum(nd$node_type == "Region"), 2L)
  expect_setequal(nd$node_key[nd$node_type == "Region"], c("cortex", "hippocampus"))
  expect_equal(sum(g$edges$data$edge_type == "sample_located_in_region"), 4L)
})

test_that("query_node_type returns Region nodes", {
  g <- make_region_graph()
  res <- as.data.frame(query_node_type(g, "Region"))
  expect_equal(nrow(res), 2L)
})

test_that("graph_from_metadata builds no Region nodes when region_id is absent", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  expect_s3_class(g, "dependency_graph")
  expect_equal(sum(g$nodes$data$node_type == "Region"), 0L)
})

test_that("derive_split_constraints(mode='region') groups samples by region", {
  g <- make_region_graph()
  constraint <- derive_split_constraints(g, mode = "region")
  expect_s3_class(constraint, "split_constraint")
  gv <- grouping_vector(constraint)
  expect_identical(unname(gv[c("S1", "S2")]), c("region:cortex", "region:cortex"))
  expect_identical(unname(gv[c("S3", "S4")]), c("region:hippocampus", "region:hippocampus"))
  expect_equal(length(unique(gv)), 2L)
  expect_identical(constraint$metadata$mode, "region")
  expect_identical(constraint$metadata$relations_used, "sample_located_in_region")
})

test_that("as_split_spec carries region_group and registers it as a block variable", {
  g <- make_region_graph()
  constraint <- derive_split_constraints(g, mode = "region")
  spec <- as_split_spec(constraint, graph = g)
  expect_true("region_group" %in% names(spec$sample_data))
  expect_false(all(is.na(spec$sample_data$region_group)))
  expect_true("region_group" %in% spec$block_vars)
  expect_silent(validate_split_spec(spec))
})

test_that("region assignments enrich a subject-mode split_spec as a blocking annotation", {
  g <- make_region_graph()
  subject_constraint <- derive_split_constraints(g, mode = "subject")
  spec <- as_split_spec(subject_constraint, graph = g)
  expect_false(all(is.na(spec$sample_data$region_group)))
  expect_true("region_group" %in% spec$block_vars)
})

test_that("composite strict grouping can traverse Region dependencies", {
  g <- make_region_graph()
  constraint <- derive_split_constraints(
    g, mode = "composite", strategy = "strict", via = c("Subject", "Region")
  )
  expect_s3_class(constraint, "split_constraint")
  expect_equal(length(unique(grouping_vector(constraint))), 2L)
})

test_that("semantic validation flags multi-region samples as an error", {
  samples <- create_nodes(data.frame(sample_id = "S1"), "Sample", "sample_id")
  regions <- create_nodes(data.frame(region_id = c("cortex", "hippocampus")), "Region", "region_id")
  region_edges <- create_edges(
    data.frame(sample_id = "S1", region_id = c("cortex", "hippocampus")),
    "sample_id", "region_id", "Sample", "Region", "sample_located_in_region"
  )
  g_multi <- build_dependency_graph(list(samples, regions), list(region_edges), validate = FALSE)
  report <- validate_graph(g_multi)
  expect_false(report$valid)
  expect_true(any(grepl("sample_located_in_region", report$errors)))
})

test_that("derive_split_constraints(mode='region') rejects ambiguous region assignments", {
  samples <- create_nodes(data.frame(sample_id = "S1"), "Sample", "sample_id")
  regions <- create_nodes(data.frame(region_id = c("cortex", "hippocampus")), "Region", "region_id")
  region_edges <- create_edges(
    data.frame(sample_id = "S1", region_id = c("cortex", "hippocampus")),
    "sample_id", "region_id", "Sample", "Region", "sample_located_in_region"
  )
  g_multi <- build_dependency_graph(list(samples, regions), list(region_edges), validate = FALSE)
  expect_error(
    derive_split_constraints(g_multi, mode = "region"),
    "Multiple region assignments found",
    fixed = TRUE
  )
})

test_that("plot renders a graph containing Region nodes", {
  g <- make_region_graph()
  expect_silent(plot(g))
})

test_that("region_group survives a split_spec JSON round-trip", {
  testthat::skip_if_not_installed("jsonlite")
  g <- make_region_graph()
  constraint <- derive_split_constraints(g, mode = "region")
  spec <- as_split_spec(constraint, graph = g)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)
  spec2 <- read_split_spec(tmp)

  expect_true("region_group" %in% names(spec2$sample_data))
  expect_identical(spec2$sample_data$region_group, spec$sample_data$region_group)
  expect_identical(names(spec2$sample_data), names(spec$sample_data))
})

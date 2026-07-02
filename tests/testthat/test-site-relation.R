make_site_graph <- function() {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P1", "P2", "P2"),
    site_id    = c("NYC", "NYC", "BOS", "BOS"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "site-demo")
}

test_that("graph_from_metadata auto-detects site_id and builds Site nodes and edges", {
  g <- make_site_graph()
  nd <- g$nodes$data
  expect_equal(sum(nd$node_type == "Site"), 2L)
  expect_setequal(nd$node_key[nd$node_type == "Site"], c("NYC", "BOS"))
  expect_equal(sum(g$edges$data$edge_type == "sample_collected_at_site"), 4L)
})

test_that("query_node_type returns Site nodes", {
  g <- make_site_graph()
  res <- as.data.frame(query_node_type(g, "Site"))
  expect_equal(nrow(res), 2L)
})

test_that("graph_from_metadata builds no Site nodes when site_id is absent", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  expect_s3_class(g, "dependency_graph")
  expect_equal(sum(g$nodes$data$node_type == "Site"), 0L)
})

test_that("derive_split_constraints(mode='site') groups samples by site", {
  g <- make_site_graph()
  constraint <- derive_split_constraints(g, mode = "site")
  expect_s3_class(constraint, "split_constraint")
  gv <- grouping_vector(constraint)
  expect_identical(unname(gv[c("S1", "S2")]), c("site:NYC", "site:NYC"))
  expect_identical(unname(gv[c("S3", "S4")]), c("site:BOS", "site:BOS"))
  expect_equal(length(unique(gv)), 2L)
  expect_identical(constraint$metadata$mode, "site")
  expect_identical(constraint$metadata$relations_used, "sample_collected_at_site")
})

test_that("as_split_spec carries site_group and registers it as a block variable", {
  g <- make_site_graph()
  constraint <- derive_split_constraints(g, mode = "site")
  spec <- as_split_spec(constraint, graph = g)
  expect_true("site_group" %in% names(spec$sample_data))
  expect_false(all(is.na(spec$sample_data$site_group)))
  expect_true("site_group" %in% spec$block_vars)
  expect_silent(validate_split_spec(spec))
})

test_that("site assignments enrich a subject-mode split_spec as a blocking annotation", {
  g <- make_site_graph()
  subject_constraint <- derive_split_constraints(g, mode = "subject")
  spec <- as_split_spec(subject_constraint, graph = g)
  # site_group is carried even though the primary grouping is subject
  expect_false(all(is.na(spec$sample_data$site_group)))
  expect_true("site_group" %in% spec$block_vars)
})

test_that("composite strict grouping can traverse Site dependencies", {
  g <- make_site_graph()
  constraint <- derive_split_constraints(
    g, mode = "composite", strategy = "strict", via = c("Subject", "Site")
  )
  expect_s3_class(constraint, "split_constraint")
  expect_equal(length(unique(grouping_vector(constraint))), 2L)
})

test_that("semantic validation flags multi-site samples as an error", {
  samples <- create_nodes(data.frame(sample_id = "S1"), "Sample", "sample_id")
  sites   <- create_nodes(data.frame(site_id = c("NYC", "BOS")), "Site", "site_id")
  site_edges <- create_edges(
    data.frame(sample_id = "S1", site_id = c("NYC", "BOS")),
    "sample_id", "site_id", "Sample", "Site", "sample_collected_at_site"
  )
  g_multi <- build_dependency_graph(list(samples, sites), list(site_edges), validate = FALSE)
  report <- validate_graph(g_multi)
  expect_false(report$valid)
  expect_true(any(grepl("sample_collected_at_site", report$errors)))
})

test_that("derive_split_constraints(mode='site') rejects ambiguous site assignments", {
  samples <- create_nodes(data.frame(sample_id = "S1"), "Sample", "sample_id")
  sites   <- create_nodes(data.frame(site_id = c("NYC", "BOS")), "Site", "site_id")
  site_edges <- create_edges(
    data.frame(sample_id = "S1", site_id = c("NYC", "BOS")),
    "sample_id", "site_id", "Sample", "Site", "sample_collected_at_site"
  )
  g_multi <- build_dependency_graph(list(samples, sites), list(site_edges), validate = FALSE)
  expect_error(
    derive_split_constraints(g_multi, mode = "site"),
    "Multiple site assignments found",
    fixed = TRUE
  )
})

test_that("plot renders a graph containing Site nodes", {
  g <- make_site_graph()
  expect_silent(plot(g))
})

test_that("site_group survives a split_spec JSON round-trip", {
  testthat::skip_if_not_installed("jsonlite")
  g <- make_site_graph()
  constraint <- derive_split_constraints(g, mode = "site")
  spec <- as_split_spec(constraint, graph = g)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)
  spec2 <- read_split_spec(tmp)

  expect_true("site_group" %in% names(spec2$sample_data))
  expect_identical(spec2$sample_data$site_group, spec$sample_data$site_group)
  expect_identical(names(spec2$sample_data), names(spec$sample_data))
})

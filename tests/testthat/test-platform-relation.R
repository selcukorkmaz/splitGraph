make_platform_graph <- function() {
  meta <- data.frame(
    sample_id   = c("S1", "S2", "S3", "S4"),
    subject_id  = c("P1", "P1", "P2", "P2"),
    platform_id = c("illumina", "illumina", "nanopore", "nanopore"),
    assay_id    = c("rnaseq", "rnaseq", "wgs", "wgs"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "platform-demo")
}

test_that("graph_from_metadata auto-detects platform_id and builds Platform nodes and edges", {
  g <- make_platform_graph()
  nd <- g$nodes$data
  expect_equal(sum(nd$node_type == "Platform"), 2L)
  expect_setequal(nd$node_key[nd$node_type == "Platform"], c("illumina", "nanopore"))
  expect_equal(sum(g$edges$data$edge_type == "sample_run_on_platform"), 4L)
})

test_that("query_node_type returns Platform nodes", {
  g <- make_platform_graph()
  res <- as.data.frame(query_node_type(g, "Platform"))
  expect_equal(nrow(res), 2L)
})

test_that("graph_from_metadata builds no Platform nodes when platform_id is absent", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  expect_s3_class(g, "dependency_graph")
  expect_equal(sum(g$nodes$data$node_type == "Platform"), 0L)
})

test_that("derive_split_constraints(mode='platform') groups samples by platform", {
  g <- make_platform_graph()
  constraint <- derive_split_constraints(g, mode = "platform")
  expect_s3_class(constraint, "split_constraint")
  gv <- grouping_vector(constraint)
  expect_identical(unname(gv[c("S1", "S2")]), c("platform:illumina", "platform:illumina"))
  expect_identical(unname(gv[c("S3", "S4")]), c("platform:nanopore", "platform:nanopore"))
  expect_equal(length(unique(gv)), 2L)
  expect_identical(constraint$metadata$mode, "platform")
  expect_identical(constraint$metadata$relations_used, "sample_run_on_platform")
})

test_that("derive_split_constraints(mode='assay') groups samples by assay", {
  g <- make_platform_graph()
  constraint <- derive_split_constraints(g, mode = "assay")
  expect_s3_class(constraint, "split_constraint")
  gv <- grouping_vector(constraint)
  expect_identical(unname(gv[c("S1", "S2")]), c("assay:rnaseq", "assay:rnaseq"))
  expect_identical(unname(gv[c("S3", "S4")]), c("assay:wgs", "assay:wgs"))
  expect_equal(length(unique(gv)), 2L)
  expect_identical(constraint$metadata$mode, "assay")
  expect_identical(constraint$metadata$relations_used, "sample_measured_by_assay")
})

test_that("as_split_spec carries platform_group / assay_group as block variables", {
  g <- make_platform_graph()

  spec_p <- as_split_spec(derive_split_constraints(g, mode = "platform"), graph = g)
  expect_true("platform_group" %in% names(spec_p$sample_data))
  expect_false(all(is.na(spec_p$sample_data$platform_group)))
  expect_true("platform_group" %in% spec_p$block_vars)

  spec_a <- as_split_spec(derive_split_constraints(g, mode = "assay"), graph = g)
  expect_true("assay_group" %in% names(spec_a$sample_data))
  expect_false(all(is.na(spec_a$sample_data$assay_group)))
  expect_true("assay_group" %in% spec_a$block_vars)
})

test_that("platform / assay assignments enrich a subject-mode split_spec", {
  g <- make_platform_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  expect_false(all(is.na(spec$sample_data$platform_group)))
  expect_false(all(is.na(spec$sample_data$assay_group)))
  expect_true(all(c("platform_group", "assay_group") %in% spec$block_vars))
})

test_that("composite strict grouping can traverse Platform dependencies", {
  g <- make_platform_graph()
  constraint <- derive_split_constraints(
    g, mode = "composite", strategy = "strict", via = c("Subject", "Platform")
  )
  expect_s3_class(constraint, "split_constraint")
  expect_equal(length(unique(grouping_vector(constraint))), 2L)
})

test_that("semantic validation flags multi-platform samples as an error", {
  samples   <- create_nodes(data.frame(sample_id = "S1"), "Sample", "sample_id")
  platforms <- create_nodes(data.frame(platform_id = c("illumina", "nanopore")), "Platform", "platform_id")
  edges <- create_edges(
    data.frame(sample_id = "S1", platform_id = c("illumina", "nanopore")),
    "sample_id", "platform_id", "Sample", "Platform", "sample_run_on_platform"
  )
  g_multi <- build_dependency_graph(list(samples, platforms), list(edges), validate = FALSE)
  report <- validate_graph(g_multi)
  expect_false(report$valid)
  expect_true(any(grepl("sample_run_on_platform", report$errors)))
})

test_that("assay_uses_platform is a valid manually-constructed edge", {
  samples   <- create_nodes(data.frame(sample_id = "S1"), "Sample", "sample_id")
  assays    <- create_nodes(data.frame(assay_id = "rnaseq"), "Assay", "assay_id")
  platforms <- create_nodes(data.frame(platform_id = "illumina"), "Platform", "platform_id")
  assay_edges <- create_edges(
    data.frame(sample_id = "S1", assay_id = "rnaseq"),
    "sample_id", "assay_id", "Sample", "Assay", "sample_measured_by_assay"
  )
  platform_edges <- create_edges(
    data.frame(assay_id = "rnaseq", platform_id = "illumina"),
    "assay_id", "platform_id", "Assay", "Platform", "assay_uses_platform"
  )
  g <- build_dependency_graph(
    list(samples, assays, platforms), list(assay_edges, platform_edges), validate = TRUE
  )
  expect_s3_class(g, "dependency_graph")
  expect_equal(sum(g$edges$data$edge_type == "assay_uses_platform"), 1L)
})

test_that("platform_group and assay_group survive a split_spec JSON round-trip", {
  testthat::skip_if_not_installed("jsonlite")
  g <- make_platform_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "platform"), graph = g)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)
  spec2 <- read_split_spec(tmp)

  expect_true(all(c("platform_group", "assay_group") %in% names(spec2$sample_data)))
  expect_identical(spec2$sample_data$platform_group, spec$sample_data$platform_group)
  expect_identical(spec2$sample_data$assay_group, spec$sample_data$assay_group)
  expect_identical(names(spec2$sample_data), names(spec$sample_data))
})

test_that("plot renders a graph containing Platform nodes", {
  g <- make_platform_graph()
  expect_silent(plot(g))
})

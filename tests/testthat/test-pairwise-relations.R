# Shared builders -------------------------------------------------------------

make_related_graph <- function(pairs, threshold = 0.1,
                               subject_ids = c("P1", "P2", "P3", "P4")) {
  meta <- data.frame(
    sample_id  = paste0("S", seq_along(subject_ids)),
    subject_id = subject_ids,
    stringsAsFactors = FALSE
  )
  samples  <- create_nodes(meta, "Sample", "sample_id")
  subjects <- create_nodes(meta, "Subject", "subject_id")
  belongs  <- create_edges(meta, "sample_id", "subject_id",
                           "Sample", "Subject", "sample_belongs_to_subject")
  rel_edges <- relatedness_edges_from_kinship(pairs, threshold = threshold)
  build_dependency_graph(list(samples, subjects), list(belongs, rel_edges))
}

make_spatial_graph <- function(coords, radius) {
  meta <- data.frame(
    sample_id  = coords$sample_id,
    subject_id = paste0("P", seq_len(nrow(coords))),
    stringsAsFactors = FALSE
  )
  samples  <- create_nodes(meta, "Sample", "sample_id")
  subjects <- create_nodes(meta, "Subject", "subject_id")
  belongs  <- create_edges(meta, "sample_id", "subject_id",
                           "Sample", "Subject", "sample_belongs_to_subject")
  adj <- spatial_edges_from_coords(coords, radius = radius)
  build_dependency_graph(list(samples, subjects), list(belongs, adj))
}

# Edge-building helpers -------------------------------------------------------

test_that("relatedness_edges_from_kinship keeps only pairs at or above threshold", {
  pairs <- data.frame(
    id1 = c("P1", "P1", "P2"),
    id2 = c("P2", "P3", "P3"),
    kinship = c(0.25, 0.02, 0.30),
    stringsAsFactors = FALSE
  )
  es <- relatedness_edges_from_kinship(pairs, threshold = 0.1)
  ed <- es$data
  expect_equal(nrow(ed), 2L)
  expect_true(all(ed$edge_type == "subject_related_to"))
  expect_setequal(ed$from, c("subject:P1", "subject:P2"))
  # kinship carried as an edge attribute
  expect_equal(sort(vapply(ed$attrs, function(a) a$kinship, numeric(1))), c(0.25, 0.30))
})

test_that("relatedness_edges_from_kinship drops self-pairs and NA metrics", {
  pairs <- data.frame(
    id1 = c("P1", "P2"),
    id2 = c("P1", "P3"),
    kinship = c(0.9, NA_real_),
    stringsAsFactors = FALSE
  )
  es <- relatedness_edges_from_kinship(pairs, threshold = 0.1)
  expect_equal(nrow(es$data), 0L)
})

test_that("spatial_edges_from_coords keeps only pairs within radius", {
  coords <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    x = c(0, 1, 9),
    y = c(0, 1, 9),
    stringsAsFactors = FALSE
  )
  es <- spatial_edges_from_coords(coords, radius = 2)
  ed <- es$data
  expect_equal(nrow(ed), 1L)
  expect_true(all(ed$edge_type == "sample_adjacent_to"))
  expect_setequal(c(ed$from, ed$to), c("sample:S1", "sample:S2"))
})

test_that("spatial_edges_from_coords errors when no coordinate columns are present", {
  coords <- data.frame(sample_id = c("S1", "S2"), stringsAsFactors = FALSE)
  expect_error(spatial_edges_from_coords(coords, radius = 1), "coordinate")
})

# Relatedness derivation ------------------------------------------------------

test_that("mode='relatedness' groups related subjects and separates the rest", {
  pairs <- data.frame(id1 = "P1", id2 = "P2", kinship = 0.25, stringsAsFactors = FALSE)
  g <- make_related_graph(pairs)
  constraint <- derive_split_constraints(g, mode = "relatedness")
  expect_s3_class(constraint, "split_constraint")

  gv <- grouping_vector(constraint)
  expect_identical(unname(gv[["S1"]]), unname(gv[["S2"]]))   # related
  expect_false(identical(gv[["S1"]], gv[["S3"]]))            # unrelated
  expect_false(identical(gv[["S3"]], gv[["S4"]]))
  expect_equal(length(unique(gv)), 3L)
  expect_identical(constraint$metadata$mode, "relatedness")
  expect_identical(constraint$metadata$relations_used, "subject_related_to")
})

test_that("relatedness grouping is transitive (P1-P2, P2-P3 => one group)", {
  pairs <- data.frame(
    id1 = c("P1", "P2"),
    id2 = c("P2", "P3"),
    kinship = c(0.25, 0.25),
    stringsAsFactors = FALSE
  )
  g <- make_related_graph(pairs)
  gv <- grouping_vector(derive_split_constraints(g, mode = "relatedness"))
  expect_identical(unname(gv[["S1"]]), unname(gv[["S2"]]))
  expect_identical(unname(gv[["S2"]]), unname(gv[["S3"]]))
  expect_false(identical(gv[["S1"]], gv[["S4"]]))
})

test_that("samples sharing a subject are always grouped together", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    stringsAsFactors = FALSE
  )
  samples  <- create_nodes(meta, "Sample", "sample_id")
  subjects <- create_nodes(meta, "Subject", "subject_id")
  belongs  <- create_edges(meta, "sample_id", "subject_id",
                           "Sample", "Subject", "sample_belongs_to_subject")
  # No relatedness edges at all.
  pairs <- data.frame(id1 = character(), id2 = character(), kinship = numeric())
  rel_edges <- relatedness_edges_from_kinship(pairs, threshold = 0.1)
  g <- build_dependency_graph(list(samples, subjects), list(belongs, rel_edges))

  gv <- grouping_vector(derive_split_constraints(g, mode = "relatedness"))
  expect_identical(unname(gv[["S1"]]), unname(gv[["S2"]]))  # same subject
  expect_false(identical(gv[["S1"]], gv[["S3"]]))
})

# Spatial derivation ----------------------------------------------------------

test_that("mode='spatial' groups adjacent samples via connected components", {
  coords <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    x = c(0, 1, 20),
    y = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  g <- make_spatial_graph(coords, radius = 2)
  gv <- grouping_vector(derive_split_constraints(g, mode = "spatial"))
  expect_identical(unname(gv[["S1"]]), unname(gv[["S2"]]))   # 1 unit apart
  expect_false(identical(gv[["S1"]], gv[["S3"]]))            # far away
  expect_equal(length(unique(gv)), 2L)
})

test_that("spatial grouping respects the samples= subset (bridge excluded)", {
  # Chain S1 - S2 - S3 by adjacency; excluding the S2 bridge must split S1/S3.
  coords <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    x = c(0, 1, 2),
    y = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  g <- make_spatial_graph(coords, radius = 1.5)

  gv_all <- grouping_vector(derive_split_constraints(g, mode = "spatial"))
  expect_equal(length(unique(gv_all)), 1L)  # all chained together

  gv_sub <- grouping_vector(
    derive_split_constraints(g, mode = "spatial", samples = c("S1", "S3"))
  )
  expect_equal(length(gv_sub), 2L)
  expect_false(identical(gv_sub[["S1"]], gv_sub[["S3"]]))
})

# split_spec + serialization --------------------------------------------------

test_that("as_split_spec carries the component grouping for pairwise modes", {
  coords <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    x = c(0, 1, 20), y = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  g <- make_spatial_graph(coords, radius = 2)
  spec <- as_split_spec(derive_split_constraints(g, mode = "spatial"), graph = g)
  expect_s3_class(spec, "split_spec")
  expect_identical(spec$group_var, "group_id")
  expect_identical(spec$recommended_resampling, "grouped_cv")
  expect_true(validate_split_spec(spec)$valid)
})

test_that("a spatial split_spec survives a JSON round-trip", {
  testthat::skip_if_not_installed("jsonlite")
  coords <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    x = c(0, 1, 20), y = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  g <- make_spatial_graph(coords, radius = 2)
  spec <- as_split_spec(derive_split_constraints(g, mode = "spatial"), graph = g)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)
  spec2 <- read_split_spec(tmp)
  expect_identical(spec2$sample_data$group_id, spec$sample_data$group_id)
  expect_identical(names(spec2$sample_data), names(spec$sample_data))
})

test_that("plot renders a graph carrying pairwise edges", {
  pairs <- data.frame(id1 = "P1", id2 = "P2", kinship = 0.25, stringsAsFactors = FALSE)
  g <- make_related_graph(pairs)
  expect_silent(plot(g))
})

edit_meta <- function() {
  data.frame(
    sample_id    = c("S1", "S2", "S3", "S4", "S5"),
    subject_id   = c("P1", "P1", "P2", "P3", "P3"),
    batch_id     = c("B1", "B1", "B2", "B2", "B3"),
    timepoint_id = c("T1", "T2", "T1", "T2", "T3"),
    time_index   = c(1, 2, 1, 2, 3),
    stringsAsFactors = FALSE
  )
}

test_that("subset_graph keeps only the requested samples and their structure", {
  g <- graph_from_metadata(edit_meta(), graph_name = "full")
  g_sub <- subset_graph(g, samples = c("S1", "S2"))

  expect_s3_class(g_sub, "dependency_graph")
  expect_setequal(g_sub$nodes$data$node_key[g_sub$nodes$data$node_type == "Sample"], c("S1", "S2"))
  expect_setequal(g_sub$nodes$data$node_key[g_sub$nodes$data$node_type == "Subject"], "P1")
  expect_setequal(g_sub$nodes$data$node_key[g_sub$nodes$data$node_type == "Batch"], "B1")
  expect_setequal(g_sub$nodes$data$node_key[g_sub$nodes$data$node_type == "Timepoint"], c("T1", "T2"))
  # Only the precedence edge between retained timepoints survives.
  prec <- g_sub$edges$data[g_sub$edges$data$edge_type == "timepoint_precedes", ]
  expect_identical(nrow(prec), 1L)
  expect_true(validate_graph(g_sub)$valid)
  expect_identical(g_sub$metadata$graph_name, "full")
  expect_identical(g_sub$metadata$subset_of, "full")
  expect_identical(g_sub$metadata$n_samples_before_subset, 5L)
  # Input untouched.
  expect_identical(nrow(g$nodes$data), 5L + 3L + 3L + 3L)
})

test_that("subset_graph matches derive_split_constraints(samples =) semantics", {
  g <- graph_from_metadata(edit_meta())
  sub <- c("S1", "S3", "S4")
  direct <- grouping_vector(derive_split_constraints(g, "composite", samples = sub))
  via_subset <- grouping_vector(derive_split_constraints(subset_graph(g, sub), "composite"))
  canon <- function(x) as.integer(match(x, unique(x)))
  expect_identical(canon(direct), canon(via_subset))
})

test_that("subset_graph rejects unknown samples with a reference error", {
  g <- graph_from_metadata(edit_meta())
  expect_error(subset_graph(g, samples = c("S1", "ghost")), class = "splitgraph_reference_error")
})

test_that("combine_graphs unions nodes and edges and renumbers edge ids", {
  m1 <- edit_meta()[1:3, ]
  m2 <- edit_meta()[3:5, ]
  g1 <- graph_from_metadata(m1, graph_name = "part1")
  g2 <- graph_from_metadata(m2, graph_name = "part2")
  g <- combine_graphs(g1, g2)

  expect_s3_class(g, "dependency_graph")
  expect_setequal(g$nodes$data$node_key[g$nodes$data$node_type == "Sample"], paste0("S", 1:5))
  expect_false(anyDuplicated(g$edges$data$edge_id) > 0)
  expect_false(anyDuplicated(paste(g$edges$data$from, g$edges$data$to, g$edges$data$edge_type)) > 0)
  expect_identical(g$metadata$graph_name, "part1")
  expect_identical(g$metadata$combined_from, c("part1", "part2"))

  # Same grouping as building from the full table.
  full <- graph_from_metadata(edit_meta())
  expect_identical(
    grouping_vector(derive_split_constraints(g, "subject"))[paste0("S", 1:5)],
    grouping_vector(derive_split_constraints(full, "subject"))[paste0("S", 1:5)]
  )
  # A list of graphs is accepted too.
  expect_identical(nrow(combine_graphs(list(g1, g2))$nodes$data), nrow(g$nodes$data))
})

test_that("combine_graphs rejects conflicting node definitions", {
  m <- data.frame(subject_id = "P1", species = "human", stringsAsFactors = FALSE)
  base <- data.frame(sample_id = "S1", subject_id = "P1", stringsAsFactors = FALSE)
  g1 <- build_dependency_graph(
    list(create_nodes(base, "Sample", "sample_id"), create_nodes(m, "Subject", "subject_id")),
    list(create_edges(base, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"))
  )
  m2 <- m
  m2$species <- "mouse"
  g2 <- build_dependency_graph(
    list(create_nodes(base, "Sample", "sample_id"), create_nodes(m2, "Subject", "subject_id")),
    list(create_edges(base, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"))
  )
  expect_error(combine_graphs(g1, g2), class = "splitgraph_ambiguity_error")
})

test_that("add_edges appends an edge set, keeps existing ids, and records provenance", {
  g <- graph_from_metadata(edit_meta())
  before_ids <- g$edges$data$edge_id
  pairs <- data.frame(id1 = "P1", id2 = "P2", kinship = 0.3, stringsAsFactors = FALSE)
  g2 <- add_edges(g, relatedness_edges_from_kinship(pairs, threshold = 0.1))

  expect_true(all(before_ids %in% g2$edges$data$edge_id))
  expect_identical(sum(g2$edges$data$edge_type == "subject_related_to"), 1L)
  expect_identical(g2$metadata$edge_sources$subject_related_to$threshold, 0.1)
  groups <- grouping_vector(derive_split_constraints(g2, "relatedness"))
  expect_identical(groups[["S1"]], groups[["S3"]])
  expect_false(groups[["S1"]] == groups[["S4"]])

  # Adding the same edges again is a no-op (exact duplicates collapse).
  g3 <- add_edges(g2, relatedness_edges_from_kinship(pairs, threshold = 0.1))
  expect_identical(nrow(g3$edges$data), nrow(g2$edges$data))

  # Edges to unknown nodes are a reference error.
  bad <- data.frame(id1 = "P1", id2 = "P99", kinship = 0.3, stringsAsFactors = FALSE)
  expect_error(add_edges(g, relatedness_edges_from_kinship(bad, threshold = 0.1)),
               class = "splitgraph_reference_error")
})

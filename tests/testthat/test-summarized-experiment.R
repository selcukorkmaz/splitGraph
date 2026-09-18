test_that("graph_from_metadata dispatches on SummarizedExperiment colData", {
  skip_if_not_installed("SummarizedExperiment")
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P1", "P2", "P2"),
    batch_id   = c("B1", "B2", "B1", "B2"),
    stringsAsFactors = FALSE
  )
  counts <- matrix(0, nrow = 5, ncol = 4, dimnames = list(paste0("g", 1:5), meta$sample_id))

  # sample ids from the assay column names
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    colData = meta[, c("subject_id", "batch_id")]
  )
  g_se <- graph_from_metadata(se, graph_name = "se")
  g_df <- graph_from_metadata(meta, graph_name = "df")
  expect_s3_class(g_se, "dependency_graph")
  expect_identical(
    grouping_vector(derive_split_constraints(g_se, "subject")),
    grouping_vector(derive_split_constraints(g_df, "subject"))
  )

  # an explicit id column, renamed through `columns`
  cd <- meta
  names(cd) <- c("specimen", "donor", "run")
  se2 <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = counts), colData = cd)
  g2 <- graph_from_metadata(se2, sample_id_col = "specimen",
                            columns = c(subject_id = "donor", batch_id = "run"))
  expect_setequal(g2$nodes$data$node_key[g2$nodes$data$node_type == "Sample"], meta$sample_id)
  expect_identical(sum(g2$nodes$data$node_type == "Batch"), 2L)

  expect_error(graph_from_metadata(se2, sample_id_col = "nope"), class = "splitgraph_reference_error")
})

test_that("graph_from_metadata rejects unsupported inputs with a schema error", {
  expect_error(graph_from_metadata(list(a = 1)), class = "splitgraph_schema_error")
  expect_error(graph_from_metadata(42), "must be a data.frame or a SummarizedExperiment")
})

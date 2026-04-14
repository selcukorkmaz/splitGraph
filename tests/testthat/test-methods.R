test_that("summary and print methods expose stable metadata", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  edges <- create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject")
  graph <- build_dependency_graph(list(samples, subjects), list(edges), graph_name = "demo")

  sample_summary <- summary(samples)
  edge_summary <- summary(edges)
  graph_summary <- summary(graph)

  expect_equal(sample_summary$schema_version, "0.1.0")
  expect_equal(edge_summary$schema_version, "0.1.0")
  expect_equal(graph_summary$schema_version, "0.1.0")
  expect_equal(graph_summary$graph_name, "demo")
  expect_true(length(capture.output(print(graph))) > 0L)
  expect_true(length(capture.output(print(samples))) > 0L)
  expect_true(length(capture.output(print(edges))) > 0L)
  expect_s3_class(as.data.frame(samples), "data.frame")
  expect_s3_class(as.data.frame(edges), "data.frame")
})

test_that("plot.dependency_graph renders with the typed layout", {
  meta <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    batch_id = c("B1", "B2", "B1"),
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  batches <- create_nodes(meta, type = "Batch", id_col = "batch_id")
  e_sub <- create_edges(meta, "sample_id", "subject_id",
                        "Sample", "Subject", "sample_belongs_to_subject")
  e_bat <- create_edges(meta, "sample_id", "batch_id",
                        "Sample", "Batch", "sample_processed_in_batch")

  graph <- build_dependency_graph(
    nodes = list(samples, subjects, batches),
    edges = list(e_sub, e_bat),
    graph_name = "plot_demo"
  )

  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)
  on.exit({
    grDevices::dev.off()
    unlink(pdf_file)
  }, add = TRUE)

  expect_silent(plot(graph))
  expect_silent(plot(graph, layout = "sugiyama"))
  expect_silent(plot(graph, show_labels = FALSE))
})

# Classed conditions (see ?splitgraph_conditions). Every splitGraph error must
# inherit from "splitgraph_error", carry a `code`, and the documented
# subclasses must be raised at their representative sites.

simple_meta <- function() {
  data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    batch_id   = c("B1", "B2", "B1"),
    stringsAsFactors = FALSE
  )
}

expect_splitgraph_error <- function(expr, class, code = NULL) {
  cond <- tryCatch(expr, error = function(e) e)
  expect_s3_class(cond, "splitgraph_error")
  expect_s3_class(cond, class)
  expect_true(is.character(cond$code) && length(cond$code) == 1L)
  if (!is.null(code)) expect_identical(cond$code, code)
  invisible(cond)
}

test_that("reference errors: unknown ids and dangling edge endpoints", {
  g <- graph_from_metadata(simple_meta())
  expect_splitgraph_error(query_neighbors(g, "sample:nope"), "splitgraph_reference_error", "unknown_node_ids")
  expect_splitgraph_error(derive_split_constraints(g, "subject", samples = "ghost"),
                          "splitgraph_reference_error", "unknown_sample_ids")

  meta <- simple_meta()
  samples <- create_nodes(meta, "Sample", "sample_id")
  edges <- create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject")
  expect_splitgraph_error(build_dependency_graph(list(samples), list(edges)),
                          "splitgraph_reference_error", "missing_target_node")
})

test_that("schema errors: unsupported node type and wrong edge signature", {
  expect_splitgraph_error(create_nodes(simple_meta(), "Widget", "sample_id"),
                          "splitgraph_schema_error", "unsupported_node_type")
  expect_splitgraph_error(
    create_edges(simple_meta(), "sample_id", "subject_id", "Sample", "Batch", "sample_belongs_to_subject"),
    "splitgraph_schema_error", "invalid_edge_signature"
  )
})

test_that("ambiguity errors: conflicting definitions and multiple assignments", {
  conflicting <- data.frame(
    subject_id = c("P1", "P1"), species = c("human", "mouse"), stringsAsFactors = FALSE
  )
  expect_splitgraph_error(create_nodes(conflicting, "Subject", "subject_id"),
                          "splitgraph_ambiguity_error", "conflicting_node_definitions")

  meta <- simple_meta()
  extra <- data.frame(sample_id = "S1", batch_id = "B2", stringsAsFactors = FALSE)
  batch_rows <- rbind(meta[, c("sample_id", "batch_id")], extra)
  g <- build_dependency_graph(
    list(create_nodes(meta, "Sample", "sample_id"), create_nodes(batch_rows, "Batch", "batch_id")),
    list(create_edges(batch_rows, "sample_id", "batch_id", "Sample", "Batch", "sample_processed_in_batch")),
    validate = FALSE
  )
  expect_splitgraph_error(derive_split_constraints(g, "batch"),
                          "splitgraph_ambiguity_error", "sample_multiple_batch_assignments")
})

test_that("validation errors are classed and carry the failure code", {
  meta <- simple_meta()
  extra <- data.frame(sample_id = "S1", batch_id = "B2", stringsAsFactors = FALSE)
  batch_rows <- rbind(meta[, c("sample_id", "batch_id")], extra)
  expect_splitgraph_error(
    build_dependency_graph(
      list(create_nodes(meta, "Sample", "sample_id"), create_nodes(batch_rows, "Batch", "batch_id")),
      list(create_edges(batch_rows, "sample_id", "batch_id", "Sample", "Batch", "sample_processed_in_batch"))
    ),
    "splitgraph_validation_error", "graph_validation_failed"
  )
})

test_that("io errors: missing file and unparseable JSON", {
  skip_if_not_installed("jsonlite")
  expect_splitgraph_error(read_split_spec(tempfile(fileext = ".json")),
                          "splitgraph_io_error", "file_not_found")
  bad <- tempfile(fileext = ".json")
  writeLines("{ not json", bad)
  on.exit(unlink(bad), add = TRUE)
  expect_splitgraph_error(read_dependency_graph(bad), "splitgraph_io_error", "json_parse_failure")

  g <- graph_from_metadata(simple_meta())
  as_graph <- tempfile(fileext = ".json")
  on.exit(unlink(as_graph), add = TRUE)
  write_dependency_graph(g, as_graph)
  expect_splitgraph_error(read_split_spec(as_graph), "splitgraph_schema_error", "unexpected_object_type")
})

test_that("plain argument errors still inherit from splitgraph_error with an NA code", {
  cond <- tryCatch(ingest_metadata(list(a = 1)), error = function(e) e)
  expect_s3_class(cond, "splitgraph_error")
  expect_true(is.na(cond$code))
  expect_match(conditionMessage(cond), "must be a data.frame")
})

test_that("package warnings are classed as splitgraph_warning", {
  meta <- data.frame(sample_id = c("S1", "S2"), outcome_value = c(0, 1), stringsAsFactors = FALSE)
  expect_warning(graph_from_metadata(meta), class = "splitgraph_warning")
})

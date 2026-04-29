make_multi_subject_graph <- function() {
  nodes <- splitGraph::graph_node_set(
    data.frame(
      node_id   = c("sample:S1", "subject:P1", "subject:P2"),
      node_type = c("Sample", "Subject", "Subject"),
      node_key  = c("S1", "P1", "P2"),
      label     = c("S1", "P1", "P2"),
      attrs     = I(list(list(), list(), list())),
      stringsAsFactors = FALSE
    )
  )

  edges <- splitGraph::graph_edge_set(
    data.frame(
      edge_id   = c("sample_belongs_to_subject:1", "sample_belongs_to_subject:2"),
      from      = c("sample:S1", "sample:S1"),
      to        = c("subject:P1", "subject:P2"),
      edge_type = c("sample_belongs_to_subject", "sample_belongs_to_subject"),
      attrs     = I(list(list(), list())),
      stringsAsFactors = FALSE
    )
  )

  splitGraph::dependency_graph(nodes = nodes, edges = edges, graph = NULL)
}

test_that("validate_graph() accepts validation_overrides as a first-class argument", {
  g <- make_multi_subject_graph()

  # Default: multi-subject samples are an error.
  default_report <- validate_graph(g)
  expect_false(default_report$valid)
  expect_true(any(default_report$issues$code == "sample_multiple_subject_assignments"))

  # With the override, the same situation is no longer flagged.
  permissive_report <- validate_graph(
    g,
    validation_overrides = list(allow_multi_subject_samples = TRUE)
  )
  expect_true(permissive_report$valid)
  expect_false(any(permissive_report$issues$code == "sample_multiple_subject_assignments"))
})

test_that("validate_graph() validates the type of validation_overrides", {
  g <- make_multi_subject_graph()

  expect_error(
    validate_graph(g, validation_overrides = "not a list"),
    "validation_overrides"
  )
  expect_error(
    validate_graph(g, validation_overrides = list(TRUE)),
    "validation_overrides"
  )
})

test_that("validation_overrides on validate_graph() temporarily overrides graph metadata", {
  g <- make_multi_subject_graph()
  # Embed a permissive override on the graph itself.
  g$metadata$validation_overrides <- list(allow_multi_subject_samples = TRUE)

  # The graph-level override applies when no explicit override is passed.
  expect_true(validate_graph(g)$valid)

  # An explicit FALSE in the call beats the graph metadata.
  strict_report <- validate_graph(
    g,
    validation_overrides = list(allow_multi_subject_samples = FALSE)
  )
  expect_false(strict_report$valid)
})

test_that("derive_split_constraints(mode='subject') respects allow_multi_subject_samples", {
  g <- make_multi_subject_graph()

  # Without the override, derivation cannot pick a single subject -> error.
  expect_error(
    derive_split_constraints(g, mode = "subject"),
    "Multiple subject assignments|allow_multi_subject_samples",
    perl = TRUE
  )

  # With the override, derivation succeeds and records that S1 was assigned
  # to one of its subjects deterministically (the first one in edge order).
  g$metadata$validation_overrides <- list(allow_multi_subject_samples = TRUE)
  constraint <- derive_split_constraints(g, mode = "subject")
  expect_s3_class(constraint, "split_constraint")
  expect_equal(nrow(constraint$sample_map), 1L)
  expect_true(constraint$sample_map$sample_id == "S1")
  expect_true(constraint$sample_map$group_id %in% c("subject:P1", "subject:P2"))
  # And we record a warning so the ambiguity is not invisible.
  warnings <- constraint$metadata$warnings %||% character()
  expect_true(any(grepl("multiple subject", warnings, ignore.case = TRUE)))
})

test_that("override does not leak across modes (batch/study/time still error on multi-assignment)", {
  nodes <- splitGraph::graph_node_set(
    data.frame(
      node_id   = c("sample:S1", "batch:B1", "batch:B2"),
      node_type = c("Sample", "Batch", "Batch"),
      node_key  = c("S1", "B1", "B2"),
      label     = c("S1", "B1", "B2"),
      attrs     = I(list(list(), list(), list())),
      stringsAsFactors = FALSE
    )
  )
  edges <- splitGraph::graph_edge_set(
    data.frame(
      edge_id   = c("sample_processed_in_batch:1", "sample_processed_in_batch:2"),
      from      = c("sample:S1", "sample:S1"),
      to        = c("batch:B1", "batch:B2"),
      edge_type = c("sample_processed_in_batch", "sample_processed_in_batch"),
      attrs     = I(list(list(), list())),
      stringsAsFactors = FALSE
    )
  )
  g <- splitGraph::dependency_graph(nodes = nodes, edges = edges, graph = NULL)
  g$metadata$validation_overrides <- list(allow_multi_subject_samples = TRUE)

  expect_error(
    derive_split_constraints(g, mode = "batch"),
    "Multiple batch assignments"
  )
})

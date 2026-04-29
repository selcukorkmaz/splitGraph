test_that("create_nodes and create_edges build valid core objects", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    batch_id = c("B1", "B1"),
    study_id = c("ST1", "ST1"),
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  edges <- create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject")

  graph <- build_dependency_graph(list(samples, subjects), list(edges))
  validation <- validate_graph(graph)

  expect_s3_class(samples, "graph_node_set")
  expect_s3_class(edges, "graph_edge_set")
  expect_s3_class(graph, "dependency_graph")
  expect_true(validation$valid)
  expect_named(samples$data$attrs[[1]], c("subject_id", "batch_id", "study_id"))
  expect_equal(igraph::vcount(as_igraph(graph)), nrow(graph$nodes$data))
  expect_equal(igraph::ecount(as_igraph(graph)), nrow(graph$edges$data))
})

test_that("compatibility aliases delegate to the primary constructors", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  # The aliases are deprecated as of 0.2.0; deprecation warnings are tested
  # in test-deprecations.R, so silence them here while we verify delegation.
  suppressWarnings({
    subjects <- new_depgraph_nodes(create_nodes(meta, type = "Subject", id_col = "subject_id")$data)
    edges <- new_depgraph_edges(
      create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject")$data
    )
    full_nodes <- new_depgraph_nodes(rbind(samples$data, subjects$data))

    graph <- new_depgraph(nodes = full_nodes, edges = edges)
    graph2 <- build_depgraph(nodes = list(samples, subjects), edges = list(edges))
  })

  expect_s3_class(subjects, "graph_node_set")
  expect_s3_class(edges, "graph_edge_set")
  expect_s3_class(graph, "dependency_graph")
  expect_s3_class(graph2, "dependency_graph")
  expect_equal(igraph::vcount(as_igraph(graph2)), 4)
})

test_that("build_dependency_graph stores validation overrides in metadata", {
  meta <- data.frame(
    sample_id = c("S1"),
    subject_id = c("P1"),
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  edges <- create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject")

  graph <- build_dependency_graph(
    list(samples, subjects),
    list(edges),
    validation_overrides = list(allow_multi_subject_samples = TRUE)
  )

  expect_true(isTRUE(graph$metadata$validation_overrides$allow_multi_subject_samples))
})

test_that("create_edges can target unprefixed node identifiers when requested", {
  meta <- data.frame(
    sample_id = "S1",
    subject_id = "P1",
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, type = "Sample", id_col = "sample_id", prefix = FALSE)
  subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
  edges <- create_edges(
    meta,
    "sample_id",
    "subject_id",
    "Sample",
    "Subject",
    "sample_belongs_to_subject",
    from_prefix = FALSE
  )

  graph <- build_dependency_graph(list(samples, subjects), list(edges))

  expect_s3_class(graph, "dependency_graph")
  expect_true(validate_graph(graph)$valid)
  expect_equal(graph$edges$data$from, "S1")
  expect_equal(graph$edges$data$to, "subject:P1")
})

test_that("create_nodes collapses exact duplicate definitions deterministically", {
  nodes <- create_nodes(
    data.frame(
      subject_id = c("P1", "P1"),
      subject_type = c("case", "case"),
      stringsAsFactors = FALSE
    ),
    type = "Subject",
    id_col = "subject_id",
    attr_cols = "subject_type"
  )

  expect_s3_class(nodes, "graph_node_set")
  expect_equal(nrow(nodes$data), 1L)
  expect_equal(nodes$data$node_id, "subject:P1")
  expect_identical(nodes$data$attrs[[1]]$subject_type, "case")
})

test_that("create_nodes rejects conflicting duplicate definitions", {
  expect_error(
    create_nodes(
      data.frame(
        subject_id = c("P1", "P1"),
        subject_type = c("case", "control"),
        stringsAsFactors = FALSE
      ),
      type = "Subject",
      id_col = "subject_id",
      attr_cols = "subject_type"
    ),
    "Conflicting node definitions found for IDs",
    fixed = TRUE
  )
})

test_that("create_edges collapses exact duplicate definitions deterministically", {
  edges <- create_edges(
    data.frame(
      sample_id = c("S1", "S1"),
      subject_id = c("P1", "P1"),
      note = c("same", "same"),
      stringsAsFactors = FALSE
    ),
    from_col = "sample_id",
    to_col = "subject_id",
    from_type = "Sample",
    to_type = "Subject",
    relation = "sample_belongs_to_subject",
    attr_cols = "note"
  )

  expect_s3_class(edges, "graph_edge_set")
  expect_equal(nrow(edges$data), 1L)
  expect_equal(edges$data$from, "sample:S1")
  expect_equal(edges$data$to, "subject:P1")
  expect_identical(edges$data$attrs[[1]]$note, "same")
})

test_that("create_edges rejects conflicting duplicate definitions", {
  expect_error(
    create_edges(
      data.frame(
        sample_id = c("S1", "S1"),
        subject_id = c("P1", "P1"),
        note = c("first", "second"),
        stringsAsFactors = FALSE
      ),
      from_col = "sample_id",
      to_col = "subject_id",
      from_type = "Sample",
      to_type = "Subject",
      relation = "sample_belongs_to_subject",
      attr_cols = "note"
    ),
    "Conflicting edge definitions found for relations",
    fixed = TRUE
  )
})

test_that("conflicting duplicate timepoint definitions cannot change derived ordering", {
  make_graph <- function(time_index_values) {
    meta <- data.frame(
      sample_id = c("S1", "S2"),
      timepoint_id = c("T1", "T2"),
      stringsAsFactors = FALSE
    )

    sample_nodes <- create_nodes(meta, type = "Sample", id_col = "sample_id")
    time_nodes <- create_nodes(
      data.frame(
        timepoint_id = c("T1", "T1", "T2"),
        time_index = time_index_values,
        stringsAsFactors = FALSE
      ),
      type = "Timepoint",
      id_col = "timepoint_id",
      attr_cols = "time_index"
    )
    time_edges <- create_edges(
      meta,
      "sample_id",
      "timepoint_id",
      "Sample",
      "Timepoint",
      "sample_collected_at_timepoint"
    )

    build_dependency_graph(list(sample_nodes, time_nodes), list(time_edges))
  }

  expect_error(
    make_graph(c(1, 9, 2)),
    "Conflicting node definitions found for IDs",
    fixed = TRUE
  )
  expect_error(
    make_graph(c(9, 1, 2)),
    "Conflicting node definitions found for IDs",
    fixed = TRUE
  )
})

test_that("low-level constructors reject malformed attrs", {
  expect_error(
    graph_node_set(
      data.frame(
        node_id = "sample:S1",
        node_type = "Sample",
        node_key = "S1",
        label = "S1",
        attrs = I(list(list("bad"))),
        stringsAsFactors = FALSE
      )
    ),
    "named lists"
  )

  expect_error(
    graph_edge_set(
      data.frame(
        edge_id = "sample_belongs_to_subject:1",
        from = "sample:S1",
        to = "subject:P1",
        edge_type = "sample_belongs_to_subject",
        attrs = I(list(list("bad"))),
        stringsAsFactors = FALSE
      )
    ),
    "named lists"
  )
})

test_that("dependency_graph rejects igraph objects that do not match node and edge tables", {
  nodes <- graph_node_set(
    data.frame(
      node_id = c("sample:S1", "subject:P1"),
      node_type = c("Sample", "Subject"),
      node_key = c("S1", "P1"),
      label = c("S1", "P1"),
      attrs = I(list(list(), list())),
      stringsAsFactors = FALSE
    )
  )

  edges <- graph_edge_set(
    data.frame(
      edge_id = "sample_belongs_to_subject:1",
      from = "sample:S1",
      to = "subject:P1",
      edge_type = "sample_belongs_to_subject",
      attrs = I(list(list())),
      stringsAsFactors = FALSE
    )
  )

  bad_graph <- igraph::make_empty_graph(n = 1, directed = TRUE)
  igraph::V(bad_graph)$name <- "sample:S1"

  expect_error(
    dependency_graph(nodes = nodes, edges = edges, graph = bad_graph),
    "does not match the supplied node and edge tables",
    fixed = TRUE
  )
})

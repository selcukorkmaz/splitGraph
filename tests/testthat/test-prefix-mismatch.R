test_that("build_dependency_graph error mentions prefix mismatch when likely", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  # Build nodes WITHOUT prefixes ...
  samples <- create_nodes(meta, "Sample", "sample_id", prefix = FALSE)
  subjects <- create_nodes(meta, "Subject", "subject_id", prefix = FALSE)

  # ... but use the default (prefixed) edges, so endpoints will reference
  # `sample:S1` while the node table only has `S1`. This is the most common
  # first-time mistake.
  edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject"
  )

  expect_error(
    build_dependency_graph(list(samples, subjects), list(edges)),
    "prefix",
    ignore.case = TRUE
  )
})

test_that("build_dependency_graph error does NOT mention prefix when no mismatch pattern", {
  # Endpoints simply absent from the node table for a non-prefix reason.
  meta <- data.frame(
    sample_id  = "S1",
    subject_id = "P1",
    stringsAsFactors = FALSE
  )
  samples <- create_nodes(meta, "Sample", "sample_id")
  edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject"
  )
  # No subject node provided at all -> still errors, but the message should
  # be the generic missing-references error, not the prefix hint.
  expect_error(
    build_dependency_graph(list(samples), list(edges)),
    "not present",
    ignore.case = TRUE
  )
})

test_that("matching prefix conventions on both sides build successfully", {
  meta <- data.frame(
    sample_id  = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  samples <- create_nodes(meta, "Sample", "sample_id", prefix = FALSE)
  subjects <- create_nodes(meta, "Subject", "subject_id", prefix = FALSE)
  edges <- create_edges(
    meta, "sample_id", "subject_id",
    "Sample", "Subject", "sample_belongs_to_subject",
    from_prefix = FALSE, to_prefix = FALSE
  )

  expect_silent(g <- build_dependency_graph(list(samples, subjects), list(edges)))
  expect_s3_class(g, "dependency_graph")
})

test_that("schema_version is stable and independent of package version", {
  # The data-model schema version must NOT change automatically when the
  # package version is bumped. Only an explicit, documented schema change
  # should bump it. This test locks the contract.
  expect_identical(splitGraph:::.depgraph_schema_version, "0.1.0")

  pkg_version <- as.character(utils::packageVersion("splitGraph"))
  expect_type(pkg_version, "character")
})

test_that("schema_version is exposed via the metadata of constructed objects", {
  meta <- data.frame(
    sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  g <- graph_from_metadata(meta)
  expect_identical(g$metadata$schema_version, splitGraph:::.depgraph_schema_version)
  expect_identical(g$nodes$schema_version, splitGraph:::.depgraph_schema_version)
  expect_identical(g$edges$schema_version, splitGraph:::.depgraph_schema_version)
})

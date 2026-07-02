skip_if_no_jsonlite <- function() testthat::skip_if_not_installed("jsonlite")

make_spec_graph <- function() {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    batch_id   = c("B1", "B2", "B1"),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "schema-demo")
}

# ---- shipped schema files ---------------------------------------------------

test_that("shipped JSON Schema files exist and are valid JSON", {
  skip_if_no_jsonlite()
  for (obj in c("dependency_graph", "split_spec")) {
    p <- splitGraph:::.depgraph_schema_path(obj)
    expect_true(nzchar(p), info = obj)
    schema <- jsonlite::fromJSON(p, simplifyVector = FALSE)
    expect_identical(schema$`$schema`, "https://json-schema.org/draft/2020-12/schema")
    expect_true(nzchar(schema$`$id`))
  }
})

# ---- $schema reference + version stamping -----------------------------------

test_that("written JSON carries a $schema reference and the current version", {
  skip_if_no_jsonlite()
  g <- make_spec_graph()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_match(raw$`$schema`, "dependency_graph\\.schema\\.json$")
  expect_identical(raw$schema_version, "0.2.0")
  expect_identical(raw$schema_version, splitGraph:::.depgraph_schema_version)
})

# ---- validate_graph_json ----------------------------------------------------

test_that("validate_graph_json passes a well-formed graph", {
  skip_if_no_jsonlite()
  g <- make_spec_graph()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)

  report <- validate_graph_json(tmp)
  expect_s3_class(report, "splitgraph_json_report")
  expect_true(report$valid)
  expect_length(report$issues, 0L)
})

test_that("validate_graph_json flags dangling edge endpoints and unknown types", {
  skip_if_no_jsonlite()
  bad <- list(
    splitGraph_object = "dependency_graph",
    schema_version = "0.2.0",
    nodes = list(list(node_id = "sample:S1", node_type = "Sample", node_key = "S1")),
    edges = list(list(
      edge_id = "e1", from = "sample:S1", to = "subject:P9",
      edge_type = "sample_belongs_to_subject"
    ))
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(jsonlite::toJSON(bad, auto_unbox = TRUE, null = "null"), tmp)

  report <- validate_graph_json(tmp)
  expect_false(report$valid)
  expect_true(any(grepl("does not reference a declared node", report$issues)))
})

test_that("validate_graph_json rejects a split_spec file", {
  skip_if_no_jsonlite()
  g <- make_spec_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)

  report <- validate_graph_json(tmp)
  expect_false(report$valid)
  expect_true(any(grepl("dependency_graph", report$issues)))
})

# ---- validate_split_spec_json -----------------------------------------------

test_that("validate_split_spec_json passes a well-formed spec and flags missing group_id", {
  skip_if_no_jsonlite()
  g <- make_spec_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  good <- tempfile(fileext = ".json")
  on.exit(unlink(good), add = TRUE)
  write_split_spec(spec, good)
  expect_true(validate_split_spec_json(good)$valid)

  bad <- list(
    splitGraph_object = "split_spec",
    schema_version = "0.2.0",
    group_var = "group_id",
    sample_data = list(list(sample_id = "S1"))  # missing group_id
  )
  bad_path <- tempfile(fileext = ".json")
  on.exit(unlink(bad_path), add = TRUE)
  writeLines(jsonlite::toJSON(bad, auto_unbox = TRUE, null = "null"), bad_path)

  report <- validate_split_spec_json(bad_path)
  expect_false(report$valid)
  expect_true(any(grepl("group_id", report$issues)))
})

# ---- schema-version compatibility -------------------------------------------

test_that("reading a same-major (0.1.0) file loads silently", {
  skip_if_no_jsonlite()
  g <- make_spec_graph()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  raw$schema_version <- "0.1.0"
  writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null"), tmp)

  expect_silent(g2 <- read_dependency_graph(tmp))
  expect_s3_class(g2, "dependency_graph")
})

# ---- migration --------------------------------------------------------------

test_that("migrate_split_spec_json upgrades an old-version file to current", {
  skip_if_no_jsonlite()
  # A hand-built 0.1.0 split_spec: old version, no $schema, and none of the
  # site/region/platform/assay columns that were added later.
  old <- list(
    splitGraph_object = "split_spec",
    schema_version = "0.1.0",
    group_var = "group_id",
    block_vars = list("batch_group"),
    time_var = NA,
    ordering_required = FALSE,
    constraint_mode = "subject",
    constraint_strategy = "subject",
    recommended_resampling = "grouped_cv",
    metadata = list(source_mode = "subject"),
    sample_data = list(
      list(sample_id = "S1", sample_node_id = "sample:S1",
           group_id = "subject:P1", primary_group = "subject:P1",
           batch_group = "B1", study_group = NA,
           timepoint_id = NA, time_index = NA, order_rank = NA)
    )
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(jsonlite::toJSON(old, auto_unbox = TRUE, null = "null"), tmp)

  migrate_split_spec_json(tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_identical(raw$schema_version, "0.2.0")
  expect_match(raw$`$schema`, "split_spec\\.schema\\.json$")
  # New columns are now present in every row.
  row1 <- raw$sample_data[[1L]]
  expect_true(all(c("site_group", "region_group", "platform_group", "assay_group") %in% names(row1)))
  # And the migrated file validates.
  expect_true(validate_split_spec_json(tmp)$valid)
})

# ---- provenance -------------------------------------------------------------

test_that("as_split_spec records derivation provenance in metadata", {
  g <- make_spec_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  expect_identical(spec$metadata$source_mode, "subject")
  expect_false(is.null(spec$metadata$splitgraph_version))
  expect_false(is.null(spec$metadata$derived_at))
})

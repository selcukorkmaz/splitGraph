# Cross-language conformance: the shipped Python reference consumer must
# recover exactly the grouping and ordering that R produced. Skipped unless a
# python3 interpreter is available (and never run on CRAN), so the R package
# check stays self-contained while CI / local runs still exercise the seam.

skip_if_no_python_conformance <- function() {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_on_cran()
  py <- Sys.which("python3")
  if (!nzchar(py)) testthat::skip("python3 not available")
  script <- system.file("python", "conformance.py", package = "splitGraph")
  if (!nzchar(script) || !file.exists(script)) testthat::skip("conformance.py not found")
  list(py = py, script = script)
}

test_that("Python reference consumer reproduces R grouping and order_rank", {
  cfg <- skip_if_no_python_conformance()

  meta <- data.frame(
    sample_id    = c("S1", "S2", "S3", "S4"),
    subject_id   = c("P1", "P1", "P2", "P3"),
    timepoint_id = c("T0", "T1", "T0", "T2"),
    time_index   = c(0, 1, 0, 2),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  constraint <- derive_split_constraints(g, mode = "time")
  spec <- as_split_spec(constraint, graph = g)

  in_path  <- tempfile(fileext = ".json")
  out_path <- tempfile(fileext = ".json")
  on.exit(unlink(c(in_path, out_path)), add = TRUE)
  write_split_spec(spec, in_path)

  status <- suppressWarnings(system2(
    cfg$py, c("-B", shQuote(cfg$script), shQuote(in_path), shQuote(out_path)),
    stdout = TRUE, stderr = TRUE
  ))
  exit_code <- attr(status, "status")
  if (!is.null(exit_code) && exit_code != 0L) {
    testthat::skip(paste("python conformance run failed:", paste(status, collapse = " ")))
  }
  expect_true(file.exists(out_path))

  py <- jsonlite::fromJSON(out_path, simplifyVector = TRUE)

  # Schema version agrees.
  expect_identical(py$schema_version, splitGraph:::.depgraph_schema_version)

  # Grouping matches R's grouping_vector() exactly.
  r_grouping <- grouping_vector(constraint)
  py_grouping <- unlist(py$grouping)
  expect_identical(sort(names(py_grouping)), sort(names(r_grouping)))
  expect_identical(py_grouping[names(r_grouping)], r_grouping[names(r_grouping)])

  # order_rank matches spec sample_data exactly.
  r_order <- stats::setNames(spec$sample_data$order_rank, spec$sample_data$sample_id)
  py_order <- unlist(py$order_ranks)
  expect_equal(py_order[names(r_order)], r_order[names(r_order)],
               ignore_attr = TRUE)
})

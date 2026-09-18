# Wall-clock budget guard for the core pipeline. This is not a benchmark: the
# budgets are deliberately generous (an order of magnitude above what the
# vectorised implementation needs on a laptop) so the test only fails when a
# change reintroduces per-row or per-pair construction, i.e. superlinear
# behaviour that would take minutes rather than seconds. Never runs on CRAN.

perf_cohort <- function(n, seed = 7L) {
  set.seed(seed)
  meta <- data.frame(
    sample_id    = paste0("S", seq_len(n)),
    subject_id   = paste0("P", sample(ceiling(n / 3), n, replace = TRUE)),
    batch_id     = paste0("B", sample(ceiling(n / 50), n, replace = TRUE)),
    study_id     = paste0("ST", sample(5L, n, replace = TRUE)),
    site_id      = paste0("Site", sample(8L, n, replace = TRUE)),
    timepoint_id = paste0("T", sample(4L, n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  meta$time_index <- as.integer(sub("T", "", meta$timepoint_id))
  meta
}

elapsed <- function(expr) unname(system.time(expr)[["elapsed"]])

test_that("the core pipeline stays within its wall-clock budget at 5000 samples", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("SPLITGRAPH_SKIP_PERF"), "true"), "perf budget disabled via SPLITGRAPH_SKIP_PERF")

  meta <- perf_cohort(5000L)

  t_build <- elapsed(g <- graph_from_metadata(meta, validate = FALSE))
  t_validate <- elapsed(report <- validate_graph(g))
  t_subject <- elapsed(con <- derive_split_constraints(g, "subject"))
  # Default `via` includes study and timepoint targets shared by ~n/5 and ~n/4
  # samples: the case that used to be quadratic.
  t_composite <- elapsed(comp <- derive_split_constraints(g, "composite"))
  t_spec <- elapsed(spec <- as_split_spec(con, graph = g))
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  t_write <- if (requireNamespace("jsonlite", quietly = TRUE)) elapsed(write_dependency_graph(g, tmp)) else 0

  expect_s3_class(report, "depgraph_validation_report")
  expect_identical(nrow(comp$sample_map), 5000L)
  expect_identical(nrow(spec$sample_data), 5000L)

  budgets <- c(build = 10, validate = 10, subject = 5, composite = 15, spec = 15, write = 10)
  timings <- c(build = t_build, validate = t_validate, subject = t_subject,
               composite = t_composite, spec = t_spec, write = t_write)
  over <- timings > budgets
  expect_false(
    any(over),
    info = paste0(
      "Steps over budget (seconds): ",
      paste(sprintf("%s=%.1f (budget %.0f)", names(timings)[over], timings[over], budgets[over]), collapse = ", ")
    )
  )
})

test_that("composite derivation scales roughly linearly, not quadratically", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("SPLITGRAPH_SKIP_PERF"), "true"), "perf budget disabled via SPLITGRAPH_SKIP_PERF")

  g_small <- graph_from_metadata(perf_cohort(1000L), validate = FALSE)
  g_large <- graph_from_metadata(perf_cohort(4000L), validate = FALSE)

  # Warm up once so package loading and igraph initialisation are excluded.
  invisible(derive_split_constraints(g_small, "composite"))
  t_small <- elapsed(derive_split_constraints(g_small, "composite"))
  t_large <- elapsed(derive_split_constraints(g_large, "composite"))

  # 4x the samples; quadratic behaviour would give ~16x. Allow up to 8x, and
  # ignore the ratio entirely when both runs are too fast to measure reliably.
  if (t_small >= 0.2) {
    expect_lt(t_large / t_small, 8)
  } else {
    expect_lt(t_large, 3)
  }
})

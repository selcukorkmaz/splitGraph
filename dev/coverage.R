# Local test coverage report (mirrors .github/workflows/test-coverage.yaml).
#
#   Rscript dev/coverage.R            # summary per file + total, fails below 90 %
#   Rscript dev/coverage.R report     # also opens the interactive HTML report
#
# Requires covr (install.packages("covr")). NOT_CRAN is set so the
# skip_on_cran() tests (Python conformance, performance budget) are included;
# set SPLITGRAPH_SKIP_PERF=true to leave the budget test out on a slow machine.

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("covr is not installed: install.packages('covr')", call. = FALSE)
}

Sys.setenv(NOT_CRAN = "true")
threshold <- 90

cov <- covr::package_coverage(".", type = "tests", quiet = TRUE)
by_file <- covr::coverage_to_list(cov)$filecoverage
total <- covr::percent_coverage(cov)

cat("\nCoverage by file (%):\n")
print(round(sort(by_file), 1))
cat(sprintf("\nTOTAL: %.1f %% (threshold %d %%)\n", total, threshold))

if (identical(commandArgs(trailingOnly = TRUE), "report")) {
  covr::report(cov)
}

if (total < threshold) {
  stop(sprintf("Coverage %.1f %% is below the %d %% threshold.", total, threshold), call. = FALSE)
}
invisible(cov)

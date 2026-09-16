# Benchmark the core splitGraph pipeline on a synthetic cohort.
#
# Usage (from the package root, after installing or with pkgload):
#   Rscript inst/bench/pipeline.R            # default sizes
#   Rscript inst/bench/pipeline.R 500 5000   # custom sizes
#
# Cohort shape (matches ROADMAP-0.4.0.md, Workstream A): subjects ~ n/3 with
# repeated samples, batches ~ n/50, 5 studies, 8 sites, 4 timepoints with a
# numeric time_index. Every step is timed once; the composite step is timed
# twice, with the default `via` (subject, batch, study, time) and with
# `via = c("subject", "batch")`, because the two differ in how many samples
# share a target.

if (!requireNamespace("splitGraph", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(splitGraph)
}

sizes <- as.integer(commandArgs(trailingOnly = TRUE))
if (length(sizes) == 0L) sizes <- c(500L, 2000L, 5000L)

make_cohort <- function(n, seed = 1L) {
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

time_it <- function(expr) round(unname(system.time(expr)[["elapsed"]]), 2)

bench_one <- function(n) {
  meta <- make_cohort(n)
  out <- c(n = n)
  out["build"]              <- time_it(g <- graph_from_metadata(meta, validate = FALSE))
  out["validate"]           <- time_it(validate_graph(g))
  out["derive_subject"]     <- time_it(con <- derive_split_constraints(g, "subject"))
  out["composite_default"]  <- time_it(derive_split_constraints(g, "composite"))
  out["composite_subj_bat"] <- time_it(derive_split_constraints(g, "composite", via = c("subject", "batch")))
  out["as_split_spec"]      <- time_it(spec <- as_split_spec(con, graph = g))
  out["write_graph"]        <- time_it(write_dependency_graph(g, tempfile(fileext = ".json")))
  out["write_spec"]         <- time_it(write_split_spec(spec, tempfile(fileext = ".json")))
  out["shared_deps_subject"] <- time_it(detect_shared_dependencies(g, via = "Subject"))
  out
}

results <- do.call(rbind, lapply(sizes, bench_one))
cat("\nsplitGraph", as.character(utils::packageVersion("splitGraph")),
    "| R", R.version$major, ".", R.version$minor, "| seconds (elapsed)\n\n", sep = "")
print(results)
invisible(results)

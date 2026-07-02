# Boundary contract with bioLeak, the primary downstream consumer. splitGraph
# emits a `split_spec`; bioLeak::as_leaksplits() consumes it. This test pins the
# seam so neither side can silently break it: it asserts that a splitGraph
# split_spec satisfies the columns and fields bioLeak reads
# (`sample_id`, `group_var`, and the optional `batch_group` / `study_group` /
# `timepoint_id` / `order_rank` block columns). Skipped when bioLeak is absent.

skip_if_no_bioleak <- function() {
  testthat::skip_if_not_installed("bioLeak")
}

make_contract_graph <- function() {
  meta <- data.frame(
    sample_id  = paste0("S", 1:12),
    subject_id = rep(paste0("P", 1:6), each = 2),
    batch_id   = rep(c("B1", "B2", "B3"), times = 4),
    stringsAsFactors = FALSE
  )
  graph_from_metadata(meta, graph_name = "contract-demo")
}

contract_data <- function(n = 12) {
  set.seed(1)
  data.frame(
    sample_id = paste0("S", seq_len(n)),
    y = rbinom(n, 1, 0.5),
    stringsAsFactors = FALSE
  )
}

test_that("a subject-mode split_spec is accepted by bioLeak::as_leaksplits()", {
  skip_if_no_bioleak()
  g <- make_contract_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)

  ls <- bioLeak::as_leaksplits(spec, data = contract_data(), outcome = "y", v = 3)
  expect_s4_class(ls, "LeakSplits")
})

test_that("a batch-mode split_spec forwards its block column to bioLeak", {
  skip_if_no_bioleak()
  g <- make_contract_graph()
  spec <- as_split_spec(derive_split_constraints(g, mode = "batch"), graph = g)

  # batch_group must be present and populated for bioLeak to pick it up.
  expect_true("batch_group" %in% names(spec$sample_data))
  expect_false(all(is.na(spec$sample_data$batch_group)))

  ls <- bioLeak::as_leaksplits(spec, data = contract_data(), outcome = "y", v = 3)
  expect_s4_class(ls, "LeakSplits")
})

test_that("newer additive columns do not break the bioLeak seam", {
  skip_if_no_bioleak()
  # A subject-mode spec on a graph that ALSO carries site structure: the
  # constraint_mode stays in bioLeak's supported set, while sample_data gains a
  # site_group annotation (a column bioLeak does not read). The seam must hold:
  # bioLeak keys on sample_id + group_var and ignores the extra annotation.
  meta <- data.frame(
    sample_id  = paste0("S", 1:12),
    subject_id = rep(paste0("P", 1:6), each = 2),
    site_id    = rep(c("NYC", "BOS", "SFO"), times = 4),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  expect_true("site_group" %in% names(spec$sample_data))
  expect_false(all(is.na(spec$sample_data$site_group)))

  ls <- bioLeak::as_leaksplits(spec, data = contract_data(), outcome = "y", v = 3)
  expect_s4_class(ls, "LeakSplits")
})

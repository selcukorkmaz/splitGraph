# Tests for v0.5.0 feature expansion:
# F1: compare_split_constraints() & find_dependency_bridges()
# F2: orthogonal time_ordering on derive_split_constraints() & stratify_by on as_split_spec()
# F3: threshold-parameterized min_weight / max_distance on derive_split_constraints()
# F4: Python SplitSpec.iter_splits() & graph_from_metadata.MultiAssayExperiment()

test_that("F1: compare_split_constraints compares candidate modes and flagsgiant-component infeasibility", {
  meta <- data.frame(
    sample_id  = paste0("S", 1:8),
    subject_id = c("P1", "P1", "P2", "P2", "P3", "P3", "P4", "P4"),
    batch_id   = rep("B1", 8),
    study_id   = rep(c("ST1", "ST2"), each = 4),
    outcome_id = rep(c("ctrl", "case"), 4),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)

  cmp <- compare_split_constraints(g, k = 2L)
  expect_s3_class(cmp, "split_constraint_comparison")
  df <- as.data.frame(cmp)
  expect_true(all(c("subject", "batch", "study", "composite") %in% df$mode))

  # Subject has 4 equal groups of size 2 -> feasible for k = 2.
  subj_row <- df[df$mode == "subject", ]
  expect_identical(subj_row$n_groups, 4L)
  expect_true(subj_row$k_fold_feasible)
  expect_equal(subj_row$effective_n_groups, 4)
  expect_identical(subj_row$min_stratum_groups, 4L)

  # Batch B1 collapses all 8 samples into 1 group -> infeasible for k = 2,
  # and a 1-group collapse does not count as severing risks in comparison.
  batch_row <- df[df$mode == "batch", ]
  expect_identical(batch_row$n_groups, 1L)
  expect_false(batch_row$k_fold_feasible)
  expect_identical(batch_row$n_risks_severed, 0L)
  expect_identical(batch_row$min_stratum_groups, 1L)

  # Study ST1/ST2 nests P1..P4 within 2 study groups -> empirically severs all 4 subject risks.
  study_row <- df[df$mode == "study", ]
  expect_identical(study_row$n_groups, 2L)
  expect_identical(study_row$n_risks_severed, 4L)

  # S3 print and summary methods work cleanly.
  expect_output(print(cmp), "<split_constraint_comparison>")
  s <- summary(cmp)
  expect_identical(s$k, 2L)
  expect_true("subject" %in% s$feasible_modes)
  expect_false("batch" %in% s$feasible_modes)
})

test_that("F1: find_dependency_bridges identifies the exact entity collapsing a composite split", {
  meta <- data.frame(
    sample_id  = paste0("S", 1:6),
    subject_id = c("P1", "P1", "P2", "P2", "P3", "P3"),
    batch_id   = c("B_bridge", "B1", "B_bridge", "B2", "B_bridge", "B3"),
    outcome_id = rep(c("ctrl", "case"), 3),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)

  # Default `via` must NOT allow Outcome nodes to short-circuit real Batch bridges.
  bridges <- find_dependency_bridges(g)
  expect_s3_class(bridges, "graph_query_result")
  expect_true(nrow(bridges$edges) > 0L)
  bdf <- as.data.frame(bridges)

  # B_bridge links all 3 subjects into 1 giant component; removing it yields 3 components.
  expect_identical(bdf$node_id[[1L]], "batch:B_bridge")
  expect_true(bdf$is_bridge[[1L]])
  expect_identical(bdf$n_components_before[[1L]], 1L)
  expect_identical(bdf$n_components_without[[1L]], 3L)
  expect_identical(bdf$components_gained[[1L]], 2L)
  expect_identical(bdf$max_size_reduction[[1L]], 4L)

  # Spatial chain S1 -- S2 -- S3: S2 is a spatial sample bridge.
  coords <- data.frame(sample_id = paste0("S", 1:6), x = c(0, 1, 2, 10, 20, 30), y = rep(0, 6))
  g_sp <- add_edges(g, spatial_edges_from_coords(coords, radius = 1.5))
  sp_bridges <- find_dependency_bridges(g_sp, via = "spatial")
  expect_identical(as.data.frame(sp_bridges)$node_id[[1L]], "sample:S2")
  expect_true(as.data.frame(sp_bridges)$is_bridge[[1L]])
})

test_that("F2: derive_split_constraints(time_ordering = TRUE) and as_split_spec(stratify_by = ...) support orthogonal roles", {
  meta <- data.frame(
    sample_id    = paste0("S", 1:6),
    subject_id   = rep(c("P1", "P2", "P3"), each = 2),
    batch_id     = rep(c("B1", "B2"), 3),
    timepoint_id = rep(c("T1", "T2"), 3),
    time_index   = rep(c(1, 2), 3),
    outcome_id   = c("ctrl", "ctrl", "case", "case", "ctrl", "case"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)

  # Subject-grouped + time-ordered: 3 subject groups (NOT collapsed by timepoint!),
  # with complete order_rank and ordering_required = TRUE.
  con_subj_time <- derive_split_constraints(g, mode = "subject", time_ordering = TRUE)
  expect_identical(con_subj_time$metadata$n_groups, 3L)
  expect_identical(con_subj_time$recommended_downstream_args$time_var, "order_rank")
  expect_true(con_subj_time$recommended_downstream_args$ordering_required)
  expect_identical(con_subj_time$sample_map$order_rank, rep(c(1L, 2L), 3))

  # Composite strict with time_ordering = TRUE excludes "time" from component
  # merging so subjects at T1 stay in separate groups when via = c("subject", "time").
  con_comp_time <- derive_split_constraints(
    g, mode = "composite", strategy = "strict",
    via = c("subject", "time"), time_ordering = TRUE
  )
  expect_identical(con_comp_time$metadata$n_groups, 3L)
  expect_true(con_comp_time$recommended_downstream_args$ordering_required)

  # stratify_by = "batch" or "none" on as_split_spec().
  spec_batch_strat <- as_split_spec(con_subj_time, graph = g, stratify_by = "batch")
  expect_identical(spec_batch_strat$stratum_var, "stratum")
  expect_identical(spec_batch_strat$sample_data$stratum, meta$batch_id)

  spec_no_strat <- as_split_spec(con_subj_time, graph = g, stratify_by = "none")
  expect_null(spec_no_strat$stratum_var)
  expect_true(all(is.na(spec_no_strat$sample_data$stratum)))
})

test_that("F3: min_weight and max_distance filter pairwise edges at constraint-derivation time", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P2", "P3"),
    stringsAsFactors = FALSE
  )
  pairs <- data.frame(
    id1 = c("P1", "P2"),
    id2 = c("P2", "P3"),
    kinship = c(0.25, 0.125),
    stringsAsFactors = FALSE
  )
  coords <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    x = c(0, 1, 4),
    y = c(0, 0, 0)
  )
  g <- add_edges(
    graph_from_metadata(meta),
    list(
      relatedness_edges_from_kinship(pairs, threshold = 0.05),
      spatial_edges_from_coords(coords, radius = 5)
    )
  )

  # Default relatedness uses all stored edges -> 1 component {S1, S2, S3}.
  c_rel_all <- derive_split_constraints(g, mode = "relatedness")
  expect_identical(c_rel_all$metadata$n_groups, 1L)

  # min_weight = 0.20 drops the 0.125 edge (P2-P3) -> 2 components: {S1, S2} and {S3}.
  c_rel_strict <- derive_split_constraints(g, mode = "relatedness", min_weight = 0.20)
  expect_identical(c_rel_strict$metadata$n_groups, 2L)
  expect_identical(c_rel_strict$metadata$threshold, 0.20)

  # max_distance = 1.5 drops the distance-3 edge (S2-S3) -> 2 components: {S1, S2} and {S3}.
  c_sp_strict <- derive_split_constraints(g, mode = "spatial", max_distance = 1.5)
  expect_identical(c_sp_strict$metadata$n_groups, 2L)
  expect_identical(c_sp_strict$metadata$threshold, 1.5)

  # Invalid thresholds error even on graphs with zero pairwise edges.
  g_no_pw <- graph_from_metadata(meta)
  expect_error(
    derive_split_constraints(g_no_pw, mode = "composite", via = c("subject", "spatial"), max_distance = -1),
    "max_distance"
  )
  expect_error(
    derive_split_constraints(g_no_pw, mode = "composite", via = c("subject", "relatedness"), min_weight = "bad"),
    "min_weight"
  )
})

test_that("F4: graph_from_metadata.MultiAssayExperiment builds a valid subject + assay graph", {
  mock_mae <- structure(
    list(
      sampleMap = data.frame(
        assay = c("rnaseq", "rnaseq", "methyl", "methyl"),
        primary = c("P1", "P2", "P1", "P2"),
        colname = c("S1_rna", "S2_rna", "S1_met", "S2_met"),
        stringsAsFactors = FALSE
      ),
      colData = data.frame(
        subject_id = c("P1", "P2"),
        study_id = c("ST1", "ST1"),
        outcome_id = c("case", "ctrl"),
        row.names = c("P1", "P2"),
        stringsAsFactors = FALSE
      )
    ),
    class = "MultiAssayExperiment"
  )

  g_mae <- graph_from_metadata(mock_mae)
  expect_s3_class(g_mae, "dependency_graph")
  expect_true(validate_graph(g_mae)$valid)
  expect_identical(sum(g_mae$nodes$data$node_type == "Sample"), 4L)
  expect_identical(sum(g_mae$nodes$data$node_type == "Subject"), 2L)
  expect_identical(sum(g_mae$nodes$data$node_type == "Assay"), 2L)
  gv <- grouping_vector(derive_split_constraints(g_mae, mode = "subject"))
  expect_identical(gv[["S1_rna"]], gv[["S1_met"]])
})

test_that("F4: Python SplitSpec.iter_splits yields leak-free grouped and temporal forward-chaining splits", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  py <- ""
  for (name in c("python3", "python")) {
    candidate <- Sys.which(name)
    if (!nzchar(candidate)) next
    probe <- tryCatch(
      suppressWarnings(system2(
        candidate, c("-c", shQuote("import sys; print(sys.version_info[0])")),
        stdout = TRUE, stderr = TRUE
      )),
      error = function(e) character()
    )
    if (is.null(attr(probe, "status")) && any(trimws(probe) == "3")) {
      py <- candidate
      break
    }
  }
  skip_if(!nzchar(py), "python3 not available")

  meta <- data.frame(
    sample_id    = paste0("S", 1:8),
    subject_id   = rep(paste0("P", 1:4), each = 2),
    timepoint_id = rep(paste0("T", 1:4), 2),
    time_index   = rep(1:4, 2),
    outcome_id   = rep(c("case", "ctrl"), each = 4),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)

  # 1. Grouped + stratified spec
  spec_grp <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
  f_grp <- tempfile(fileext = ".json")
  on.exit(unlink(f_grp), add = TRUE)
  write_split_spec(spec_grp, f_grp)

  # 2. Time-ordered spec
  spec_time <- as_split_spec(derive_split_constraints(g, mode = "time"), graph = g)
  f_time <- tempfile(fileext = ".json")
  on.exit(unlink(f_time), add = TRUE)
  write_split_spec(spec_time, f_time)

  # 3. Longitudinal subject + time_ordering = TRUE panel where subjects span timepoints
  meta_panel <- data.frame(
    sample_id    = paste0("S", 1:8),
    subject_id   = rep(c("P1", "P2"), each = 4),
    timepoint_id = rep(paste0("T", 1:4), 2),
    time_index   = rep(1:4, 2),
    stringsAsFactors = FALSE
  )
  g_panel <- graph_from_metadata(meta_panel)
  spec_panel <- as_split_spec(derive_split_constraints(g_panel, mode = "subject", time_ordering = TRUE), graph = g_panel)
  f_panel <- tempfile(fileext = ".json")
  on.exit(unlink(f_panel), add = TRUE)
  write_split_spec(spec_panel, f_panel)

  py_dir <- system.file("python", package = "splitGraph", mustWork = TRUE)
  code <- sprintf(
    "import sys; sys.path.insert(0, %s); from splitspec.reader import load_split_spec; "
    , shQuote(py_dir)
  )
  code <- paste0(
    code,
    sprintf("s1 = load_split_spec(%s); splits1 = list(s1.iter_splits(n_splits=2)); ", shQuote(f_grp)),
    "assert len(splits1) == 2; ",
    "g1 = s1.groups(); ",
    "assert all(set(g1[i] for i in tr).isdisjoint(set(g1[i] for i in te)) for tr, te in splits1); ",
    sprintf("s2 = load_split_spec(%s); splits2 = list(s2.iter_splits(n_splits=2)); ", shQuote(f_time)),
    "assert len(splits2) == 2; ",
    "r2 = s2.order_ranks(); ",
    "assert all(max(r2[i] for i in tr) < min(r2[i] for i in te) for tr, te in splits2); ",
    sprintf("s3 = load_split_spec(%s); splits3 = list(s3.iter_splits(n_splits=2)); ", shQuote(f_panel)),
    "g3 = s3.groups(); r3 = s3.order_ranks(); ",
    "assert all(len(tr) > 0 and len(te) > 0 for tr, te in splits3); ",
    "assert all(set(g3[i] for i in tr).isdisjoint(set(g3[i] for i in te)) for tr, te in splits3); ",
    "assert all(max(r3[i] for i in tr) < min(r3[i] for i in te) for tr, te in splits3); ",
    "print('OK')"
  )
  res <- system2(py, c("-c", shQuote(code)), stdout = TRUE, stderr = TRUE)
  expect_identical(tail(res, 1L), "OK")
})

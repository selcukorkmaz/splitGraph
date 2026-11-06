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
  res <- system2(py, c("-B", "-c", shQuote(code)), stdout = TRUE, stderr = TRUE)
  expect_identical(tail(res, 1L), "OK")
})

test_that("B04 / D08: summarize_leakage_risks and compare_split_constraints verify subject risks against actual sample_map assignments", {
  # 1. D08: multi-subject override where s1 is assigned to p1 while also linked to p2 (with s2, s3)
  g_d08 <- graph_from_metadata(
    data.frame(
      sample_id = c("s1", "s2", "s3"),
      subject_id = c("p1", "p2", "p2"),
      stringsAsFactors = FALSE
    ),
    validation_overrides = list(allow_multi_subject_samples = TRUE)
  )
  edge <- create_edges(
    data.frame(s = "s1", p = "p2", stringsAsFactors = FALSE),
    "s", "p", "Sample", "Subject", "sample_belongs_to_subject"
  )
  g_d08 <- add_edges(g_d08, edge)
  con_d08 <- derive_split_constraints(g_d08, "subject")
  diag_d08 <- as.data.frame(summarize_leakage_risks(g_d08, constraint = con_d08))
  expect_identical(diag_d08$severed[diag_d08$category == "repeated_subject_samples"], FALSE)

  cmp_d08 <- as.data.frame(compare_split_constraints(g_d08, modes = "subject", k = 2L))
  expect_identical(cmp_d08$n_risks_severed, 0L)
  expect_identical(cmp_d08$n_risks_remaining, 1L)

  # 2. Complete same-group assignments return TRUE; different-group assignments return FALSE
  g_site <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2", "s3", "s4"),
    subject_id = c("p1", "p1", "p2", "p2"),
    site_id = c("site1", "site2", "site1", "site2"),
    study_id = c("st1", "st2", "st1", "st2"),
    stringsAsFactors = FALSE
  ))
  diag_subj <- as.data.frame(summarize_leakage_risks(g_site, constraint = derive_split_constraints(g_site, "subject")))
  expect_true(all(diag_subj$severed[diag_subj$category %in% c(
    "repeated_subject_samples", "subject_cross_study_overlap", "subject_cross_site_overlap"
  )]))
  diag_site <- as.data.frame(summarize_leakage_risks(g_site, constraint = derive_split_constraints(g_site, "site")))
  expect_true(all(!diag_site$severed[diag_site$category %in% c(
    "repeated_subject_samples", "subject_cross_study_overlap", "subject_cross_site_overlap"
  )]))

  # 3. Incomplete coverage: NA when <= 1 affected sample is covered without observed split;
  # FALSE when observed assignments already split the covered affected samples.
  con_one_sample <- derive_split_constraints(g_site, "subject", samples = c("s1", "s3"))
  diag_incomplete <- as.data.frame(summarize_leakage_risks(g_site, constraint = con_one_sample))
  expect_true(all(is.na(diag_incomplete$severed[diag_incomplete$category == "repeated_subject_samples"])))

  con_split_sub <- derive_split_constraints(g_d08, "subject", samples = c("s1", "s2"))
  diag_split_sub <- as.data.frame(summarize_leakage_risks(g_d08, constraint = con_split_sub))
  expect_identical(diag_split_sub$severed[diag_split_sub$category == "repeated_subject_samples"], FALSE)

  # 4. Explicit subset validation evaluates issues on that subset (s2, s3 share p2; s4 is p3 -> 2 groups, severed = TRUE)
  g_sub_ok <- add_edges(
    graph_from_metadata(
      data.frame(
        sample_id = c("s1", "s2", "s3", "s4"),
        subject_id = c("p1", "p2", "p2", "p3"),
        stringsAsFactors = FALSE
      ),
      validation_overrides = list(allow_multi_subject_samples = TRUE)
    ),
    edge
  )
  con_sub_ok <- derive_split_constraints(g_sub_ok, "subject", samples = c("s2", "s3", "s4"))
  val_sub_ok <- validate_graph(subset_graph(g_sub_ok, samples = c("s2", "s3", "s4"), validate = FALSE))
  diag_sub_ok <- as.data.frame(summarize_leakage_risks(g_sub_ok, constraint = con_sub_ok, validation = val_sub_ok))
  expect_identical(diag_sub_ok$severed[diag_sub_ok$category == "repeated_subject_samples"], TRUE)
  cmp_sub_ok <- as.data.frame(compare_split_constraints(g_sub_ok, modes = "subject", samples = c("s2", "s3", "s4"), k = 2L))
  expect_identical(cmp_sub_ok$n_risks_severed, 1L)
  expect_identical(cmp_sub_ok$n_risks_remaining, 0L)
})

test_that("B09 / D01: unlinked singleton keys never collide with linked entity keys", {
  g_d01 <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2"),
    subject_id = c(NA_character_, "unlinked:s1"),
    stringsAsFactors = FALSE
  ))
  con_d01 <- derive_split_constraints(g_d01, "subject")
  gv_d01 <- grouping_vector(con_d01)
  expect_identical(length(unique(gv_d01)), 2L)
  expect_identical(con_d01$metadata$n_groups, 2L)
  expect_identical(length(unique(con_d01$sample_map$group_label)), 2L)

  # Collision with label prefix "unlinked_s1" is also disambiguated
  g_d01_lbl <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2"),
    subject_id = c(NA_character_, "unlinked_s1"),
    stringsAsFactors = FALSE
  ))
  con_d01_lbl <- derive_split_constraints(g_d01_lbl, "subject")
  expect_identical(length(unique(con_d01_lbl$sample_map$group_id)), 2L)
  expect_identical(length(unique(con_d01_lbl$sample_map$group_label)), 2L)

  # Rule-based composite mode disambiguates unlinked singletons too
  con_comp <- derive_split_constraints(g_d01, mode = "composite", strategy = "rule_based", via = "subject")
  expect_identical(length(unique(con_comp$sample_map$group_id)), 2L)

  # Ordinary unlinked samples preserve the standard "subject:unlinked:<sample_id>" key
  g_ordinary <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2"),
    subject_id = c("p1", NA_character_),
    stringsAsFactors = FALSE
  ))
  con_ordinary <- derive_split_constraints(g_ordinary, "subject")
  expect_identical(
    con_ordinary$sample_map$group_id[con_ordinary$sample_map$sample_id == "s2"],
    "subject:unlinked:s2"
  )

  # JSON round-trip preserves distinct groups
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    spec_d01 <- as_split_spec(con_d01, graph = g_d01)
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    write_split_spec(spec_d01, tmp)
    spec_rt <- read_split_spec(tmp, validate = TRUE)
    expect_identical(length(unique(spec_rt$sample_data$group_id)), 2L)
  }
})

test_that("B12 / D02: double-precision time_index values are preserved without character round-trip collapse", {
  near <- c(1, 1 + 2 * .Machine$double.eps)
  expect_identical(length(unique(near)), 2L)

  g_d02 <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2", "s3", "s4"),
    subject_id = c("p1", "p1", "p2", "p2"),
    timepoint_id = c("a", "b", "a", "c_na"),
    time_index = c(near, near[1], NA_real_),
    stringsAsFactors = FALSE
  ))
  val_d02 <- validate_graph(g_d02)
  expect_false("non_unique_subject_timepoints" %in% val_d02$issues$code)

  con_d02 <- derive_split_constraints(g_d02, "time")
  expect_identical(con_d02$sample_map$order_rank, c(1L, 2L, 1L, NA_integer_))
  expect_identical(con_d02$sample_map$time_index, c(near, near[1], NA_real_))

  # Character numeric strings remain supported
  g_chr <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2"),
    timepoint_id = c("t1", "t2"),
    time_index = c("1.25", "2.5"),
    stringsAsFactors = FALSE
  ))
  con_chr <- derive_split_constraints(g_chr, "time")
  expect_identical(con_chr$sample_map$order_rank, c(1L, 2L))
  expect_identical(con_chr$sample_map$time_index, c(1.25, 2.5))

  # Nonfinite time_index values (Inf, -Inf, NaN) emit invalid_time_index warning and become NA
  g_nonfinite <- graph_from_metadata(data.frame(
    sample_id = c("s1", "s2", "s3", "s4"),
    timepoint_id = c("t1", "t2", "t3", "t4"),
    time_index = c(1, Inf, -Inf, NaN),
    stringsAsFactors = FALSE
  ))
  val_nf <- validate_graph(g_nonfinite)
  expect_identical(sum(val_nf$issues$code == "invalid_time_index"), 3L)
  con_nf <- derive_split_constraints(g_nonfinite, "time")
  expect_identical(con_nf$sample_map$order_rank, c(1L, NA_integer_, NA_integer_, NA_integer_))
  expect_identical(con_nf$sample_map$time_index, c(1, NA_real_, NA_real_, NA_real_))

  # JSON round-trip preserves closely spaced double precision
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    g_near <- graph_from_metadata(data.frame(
      sample_id = c("s1", "s2"),
      timepoint_id = c("a", "b"),
      time_index = near,
      stringsAsFactors = FALSE
    ))
    fg <- tempfile(fileext = ".json")
    fs <- tempfile(fileext = ".json")
    on.exit(unlink(c(fg, fs)), add = TRUE)
    write_dependency_graph(g_near, fg)
    g_near_rt <- read_dependency_graph(fg, validate = TRUE)
    expect_identical(derive_split_constraints(g_near_rt, "time")$sample_map$order_rank, c(1L, 2L))

    spec_near <- as_split_spec(derive_split_constraints(g_near, "time"), graph = g_near)
    write_split_spec(spec_near, fs)
    spec_near_rt <- read_split_spec(fs, validate = TRUE)
    expect_identical(spec_near_rt$sample_data$time_index, near)
    expect_identical(spec_near_rt$sample_data$order_rank, c(1L, 2L))
  }
})

test_that("B03 & B15 (D09, D10, D11): Python SplitSpec enforces forward-split contract and schema_version validation", {
  skip_on_cran()
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

  py_dir <- system.file("python", package = "splitGraph", mustWork = TRUE)
  py_script <- tempfile(fileext = ".py")
  on.exit(unlink(py_script), add = TRUE)
  writeLines(c(
    sprintf("import sys; sys.path.insert(0, %s)", shQuote(py_dir)),
    "from splitspec.reader import SplitSpec",
    "",
    "def expect_val_err(fn, substr=None):",
    "    try:",
    "        fn()",
    "    except ValueError as exc:",
    "        if substr is not None:",
    "            assert substr in str(exc), f'Expected {substr!r} in {str(exc)!r}'",
    "        return",
    "    raise AssertionError('Expected ValueError')",
    "",
    "# 1. Original two-rank rejection with n_splits=2",
    "two_rank = SplitSpec({",
    "    'splitGraph_object': 'split_spec', 'schema_version': '0.3.0',",
    "    'group_var': 'group_id', 'time_var': 'order_rank',",
    "    'ordering_required': True, 'recommended_resampling': 'forward',",
    "    'sample_data': [",
    "        {'sample_id': 's1', 'group_id': 'g1', 'order_rank': 1},",
    "        {'sample_id': 's2', 'group_id': 'g2', 'order_rank': 2},",
    "    ],",
    "})",
    "expect_val_err(lambda: list(two_rank.iter_splits(n_splits=2)), 'distinct order_rank')",
    "",
    "# 2. Valid recurring-subject longitudinal panel (12 rows, 4 subjects x 3 timepoints)",
    "panel_rows = []",
    "for p in range(1, 5):",
    "    for t in range(1, 4):",
    "        panel_rows.append({",
    "            'sample_id': f'S{(p-1)*3+t}',",
    "            'group_id': f'subject:P{p}',",
    "            'order_rank': t,",
    "        })",
    "panel = SplitSpec({",
    "    'splitGraph_object': 'split_spec', 'schema_version': '0.3.0',",
    "    'group_var': 'group_id', 'time_var': 'order_rank',",
    "    'ordering_required': True, 'recommended_resampling': 'forward',",
    "    'sample_data': panel_rows,",
    "})",
    "splits = list(panel.iter_splits(n_splits=2, purge_ranks=0))",
    "assert splits == [([3, 9], [1, 7]), ([0, 1, 6, 7], [5, 11])]",
    "",
    "# 3. D10a: One-group panel that empties training",
    "one_grp = SplitSpec({",
    "    'splitGraph_object': 'split_spec', 'schema_version': '0.3.0',",
    "    'group_var': 'group_id', 'time_var': 'order_rank',",
    "    'ordering_required': True, 'recommended_resampling': 'forward',",
    "    'sample_data': [",
    "        {'sample_id': f's{i}', 'group_id': 'subject:p1', 'order_rank': i}",
    "        for i in (1, 2, 3)",
    "    ],",
    "})",
    "expect_val_err(lambda: list(one_grp.iter_splits(n_splits=2)), 'empty training set')",
    "",
    "# 4. D10b: Fallback empties test window on split 3 of 3; zero partial splits emitted",
    "late_fail = SplitSpec({",
    "    'splitGraph_object': 'split_spec', 'schema_version': '0.3.0',",
    "    'group_var': 'group_id', 'time_var': 'order_rank',",
    "    'ordering_required': True, 'recommended_resampling': 'forward',",
    "    'sample_data': [",
    "        {'sample_id': f's{i}', 'group_id': g, 'order_rank': r}",
    "        for i, (g, r) in enumerate(zip(['subject:p1'] * 4 + ['subject:p2'] * 4, [1, 2, 3, 4] * 2))",
    "    ],",
    "})",
    "emitted = []",
    "def consume_late():",
    "    for item in late_fail.iter_splits(n_splits=3):",
    "        emitted.append(item)",
    "expect_val_err(consume_late, 'Forward split 3/3')",
    "assert emitted == [], f'Expected no partial splits, got {emitted}'",
    "",
    "# 5. D09: Invalid n_splits and purge_ranks (including booleans and negative values)",
    "for bad_n in (1, 0, -2, True, False, 2.5, '2'):",
    "    expect_val_err(lambda n=bad_n: list(panel.iter_splits(n_splits=n)), 'n_splits')",
    "for bad_p in (-1, -0.5, True, False, float('inf'), float('nan'), '0'):",
    "    expect_val_err(lambda p=bad_p: list(panel.iter_splits(n_splits=2, purge_ranks=p)), 'purge_ranks')",
    "",
    "# 6. Missing or unusable group/rank values",
    "bad_rank = SplitSpec({",
    "    'splitGraph_object': 'split_spec', 'schema_version': '0.3.0',",
    "    'group_var': 'group_id', 'time_var': 'order_rank',",
    "    'ordering_required': True, 'recommended_resampling': 'forward',",
    "    'sample_data': [",
    "        {'sample_id': 's1', 'group_id': 'g1', 'order_rank': True},",
    "        {'sample_id': 's2', 'group_id': 'g2', 'order_rank': 2},",
    "        {'sample_id': 's3', 'group_id': 'g3', 'order_rank': 3},",
    "    ],",
    "})",
    "expect_val_err(lambda: list(bad_rank.iter_splits(n_splits=2)), 'order ranks')",
    "",
    "# 7. D11: Missing or malformed schema_version rejected; valid older 0.1.0 / 0.2.0 accepted",
    "for bad_v in (None, 3, '', '0.3', 'v0.3.0'):",
    "    payload = {'splitGraph_object': 'split_spec', 'group_var': 'group_id', 'sample_data': []}",
    "    if bad_v is not None:",
    "        payload['schema_version'] = bad_v",
    "    expect_val_err(lambda p=payload: SplitSpec(p), 'schema_version')",
    "expect_val_err(lambda: SplitSpec({'splitGraph_object': 'split_spec', 'schema_version': '1.0.0', 'group_var': 'group_id', 'sample_data': []}), 'major version')",
    "for ok_v in ('0.1.0', '0.2.0', '0.3.0'):",
    "    s_ok = SplitSpec({'splitGraph_object': 'split_spec', 'schema_version': ok_v, 'group_var': 'group_id', 'sample_data': []})",
    "    assert s_ok.schema_version == ok_v",
    "print('PYTHON_CONTRACT_OK')"
  ), py_script)

  out <- system2(py, c("-B", shQuote(py_script)), stdout = TRUE, stderr = TRUE)
  expect_identical(tail(out, 1L), "PYTHON_CONTRACT_OK")
})

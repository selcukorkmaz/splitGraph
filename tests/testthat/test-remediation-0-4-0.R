# Regression tests for the 13 confirmed defects fixed in the 0.4.0 stabilization pass.

test_that("Defect #1: leakage severance is mode-accurate for cross-site/study, relatedness, rule_based composite, and missing_time_ordering", {
  meta <- data.frame(
    sample_id    = paste0("S", 1:6),
    subject_id   = c("P1", "P1", "P2", "P2", "P3", "P4"),
    study_id     = c("ST1", "ST2", "ST1", "ST1", "ST2", "ST2"),
    site_id      = c("NYC", "BOS", "NYC", "NYC", "BOS", "BOS"),
    batch_id     = rep("B1", 6),
    timepoint_id = paste0("T", 1:6),
    stringsAsFactors = FALSE
  )
  pairs <- data.frame(id1 = "P3", id2 = "P4", kinship = 0.25, stringsAsFactors = FALSE)
  g <- add_edges(
    graph_from_metadata(meta, time_precedence = FALSE),
    relatedness_edges_from_kinship(pairs, threshold = 0.1)
  )

  # 1. Splitting by study or site does NOT sever subject_cross_study_overlap or subject_cross_site_overlap.
  d_study <- as.data.frame(summarize_leakage_risks(g, constraint = derive_split_constraints(g, "study")))
  expect_false(d_study$severed[d_study$category == "subject_cross_study_overlap"])
  expect_false(d_study$severed[d_study$category == "subject_cross_site_overlap"])

  d_site <- as.data.frame(summarize_leakage_risks(g, constraint = derive_split_constraints(g, "site")))
  expect_false(d_site$severed[d_site$category == "subject_cross_site_overlap"])
  expect_false(d_site$severed[d_site$category == "subject_cross_study_overlap"])

  # 2. Splitting by subject or relatedness DOES sever repeated_subject_samples,
  #    subject_cross_study_overlap, and subject_cross_site_overlap.
  for (m in c("subject", "relatedness")) {
    dm <- as.data.frame(summarize_leakage_risks(g, constraint = derive_split_constraints(g, m)))
    expect_true(all(dm$severed[dm$category == "repeated_subject_samples"]), info = m)
    expect_true(all(dm$severed[dm$category == "subject_cross_study_overlap"]), info = m)
    expect_true(all(dm$severed[dm$category == "subject_cross_site_overlap"]), info = m)
  }

  # 3. Rule-based composite where every sample is assigned by subject does NOT
  #    sever heavy_batch_reuse even though "batch" is in `via` and `priority`.
  c_rule <- derive_split_constraints(
    g, mode = "composite", strategy = "rule_based",
    via = c("subject", "batch"), priority = c("subject", "batch")
  )
  d_rule <- as.data.frame(summarize_leakage_risks(g, constraint = c_rule))
  expect_true(all(d_rule$severed[d_rule$category == "repeated_subject_samples"]))
  expect_false(any(d_rule$severed[d_rule$category == "heavy_batch_reuse"]))

  # 4. Strict composite with relatedness and batch severs both subject risks and heavy_batch_reuse.
  c_strict <- derive_split_constraints(g, mode = "composite", strategy = "strict", via = c("relatedness", "batch"))
  d_strict <- as.data.frame(summarize_leakage_risks(g, constraint = c_strict))
  expect_true(all(d_strict$severed[d_strict$category == "repeated_subject_samples"]))
  expect_true(all(d_strict$severed[d_strict$category == "subject_cross_site_overlap"]))
  expect_true(all(d_strict$severed[d_strict$category == "heavy_batch_reuse"]))

  # 5. missing_time_ordering is never severed when no time ordering metadata exist.
  d_time <- as.data.frame(summarize_leakage_risks(g, constraint = derive_split_constraints(g, "time")))
  expect_false(d_time$severed[d_time$category == "missing_time_ordering"])
})

test_that("Defect #2: disconnected/partial timepoint_precedes DAGs do not fabricate total order, and transitive time_index conflicts are caught", {
  # 4 timepoints, only 1 precedence edge T1 -> T2 (T3, T4 isolated).
  s_nodes <- create_nodes(data.frame(sample_id = paste0("S", 1:4)), "Sample", "sample_id")
  t_nodes <- create_nodes(data.frame(timepoint_id = paste0("T", 1:4)), "Timepoint", "timepoint_id")
  e_st <- create_edges(
    data.frame(s = paste0("S", 1:4), tp = paste0("T", 1:4)),
    "s", "tp", "Sample", "Timepoint", "sample_collected_at_timepoint"
  )
  e_tp <- create_edges(
    data.frame(f = "T1", t = "T2"),
    "f", "t", "Timepoint", "Timepoint", "timepoint_precedes"
  )
  g_partial <- build_dependency_graph(list(s_nodes, t_nodes), list(e_st, e_tp))
  con_partial <- derive_split_constraints(g_partial, mode = "time")

  expect_false(con_partial$recommended_downstream_args$ordering_required)
  expect_true(all(is.na(con_partial$sample_map$order_rank)))
  expect_true(any(grepl("partial ordering", con_partial$metadata$warnings)))

  # Transitive conflict: T1 (time_index = 2) -> T2 (NA) -> T3 (time_index = 1).
  t_trans <- create_nodes(
    data.frame(timepoint_id = c("T1", "T2", "T3"), time_index = c(2, NA, 1)),
    "Timepoint", "timepoint_id", attr_cols = "time_index"
  )
  e_trans <- create_edges(
    data.frame(f = c("T1", "T2"), t = c("T2", "T3")),
    "f", "t", "Timepoint", "Timepoint", "timepoint_precedes"
  )
  s_trans <- create_nodes(data.frame(sample_id = c("S1", "S2", "S3")), "Sample", "sample_id")
  e_st3 <- create_edges(
    data.frame(s = c("S1", "S2", "S3"), tp = c("T1", "T2", "T3")),
    "s", "tp", "Sample", "Timepoint", "sample_collected_at_timepoint"
  )
  expect_error(
    build_dependency_graph(list(s_trans, t_trans), list(e_st3, e_trans)),
    class = "splitgraph_validation_error"
  )
})

test_that("Defect #4: graph_from_metadata handles tied and character time_index without self-inflicted conflicts", {
  meta_tied <- data.frame(
    sample_id    = c("S1", "S2", "S3"),
    subject_id   = c("P1", "P2", "P3"),
    timepoint_id = c("T0_A", "T0_B", "T1"),
    time_index   = c(0, 0, 1),
    stringsAsFactors = FALSE
  )
  g_tied <- graph_from_metadata(meta_tied)
  expect_true(validate_graph(g_tied)$valid)
  con_tied <- derive_split_constraints(g_tied, mode = "time")
  expect_identical(con_tied$sample_map$order_rank, c(1L, 1L, 2L))

  meta_chr <- data.frame(
    sample_id    = c("S1", "S2"),
    subject_id   = c("P1", "P2"),
    timepoint_id = c("T2", "T10"),
    time_index   = c("2", "10"),
    stringsAsFactors = FALSE
  )
  g_chr <- graph_from_metadata(meta_chr)
  expect_true(validate_graph(g_chr)$valid)
  con_chr <- derive_split_constraints(g_chr, mode = "time")
  expect_identical(con_chr$sample_map$order_rank, c(1L, 2L))
})

test_that("Defect #5 & #12: optional attribute auto-capture, combine_graphs attr merging, col_map collisions, and outcome_scope = subject", {
  # Multi-site study with shared optional attr `institution` varying by site.
  meta_multi <- data.frame(
    sample_id          = c("S1", "S2"),
    subject_id         = c("P1", "P2"),
    study_id           = c("ST1", "ST1"),
    site_id            = c("NYC", "BOS"),
    institution        = c("HospA", "HospB"),
    outcome_id         = c("case", "case"),
    outcome_value      = c("1.2", "3.4"),
    stringsAsFactors   = FALSE
  )
  g_multi <- graph_from_metadata(meta_multi)
  expect_s3_class(g_multi, "dependency_graph")

  # combine_graphs over same samples with different metadata columns merges disjoint attrs.
  g_a <- graph_from_metadata(data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2")))
  g_b <- graph_from_metadata(data.frame(sample_id = c("S1", "S2"), batch_id = c("B1", "B2")))
  g_ab <- combine_graphs(g_a, g_b)
  expect_s3_class(g_ab, "dependency_graph")
  s1_attrs <- g_ab$nodes$data$attrs[[which(g_ab$nodes$data$node_id == "sample:S1")]]
  expect_identical(s1_attrs$subject_id, "P1")
  expect_identical(s1_attrs$batch_id, "B1")

  # col_map creating duplicate column names errors with splitgraph_input_error.
  df_dup <- data.frame(id = c("S1", "S2"), sample_id = c("X1", "X2"))
  expect_error(
    ingest_metadata(df_dup, col_map = c(sample_id = "id")),
    class = "splitgraph_input_error"
  )

  # outcome_scope = "subject" without subject_id errors; with partial NA subject_id drops incomplete links cleanly.
  expect_error(
    graph_from_metadata(data.frame(sample_id = c("S1", "S2"), outcome_id = c("a", "b")), outcome_scope = "subject"),
    class = "splitgraph_input_error"
  )
  g_part <- graph_from_metadata(
    data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", NA), outcome_id = c("a", "b")),
    outcome_scope = "subject"
  )
  expect_identical(sum(g_part$edges$data$edge_type == "subject_has_outcome"), 1L)
})

test_that("Defect #6: validate_graph_json and validate_split_spec_json handle missing required arrays and non-object rows without crashing", {
  skip_if_not_installed("jsonlite")
  tf1 <- tempfile(fileext = ".json")
  on.exit(unlink(tf1), add = TRUE)
  writeLines('{"splitGraph_object": "dependency_graph", "schema_version": "0.3.0"}', tf1)
  rep1 <- validate_graph_json(tf1)
  expect_false(rep1$valid)
  expect_true(any(grepl("`nodes` must be an array", rep1$issues)))
  expect_true(any(grepl("`edges` must be an array", rep1$issues)))

  tf2 <- tempfile(fileext = ".json")
  on.exit(unlink(tf2), add = TRUE)
  writeLines('{"splitGraph_object": "dependency_graph", "schema_version": "0.3.0", "nodes": {"node_id": "sample:S1"}, "edges": ["bad"]}', tf2)
  rep2 <- validate_graph_json(tf2)
  expect_false(rep2$valid)
  expect_true(any(grepl("`nodes` must be an array", rep2$issues)))
  expect_true(any(grepl("edge\\[1\\]: must be a JSON object", rep2$issues)))

  tf3 <- tempfile(fileext = ".json")
  on.exit(unlink(tf3), add = TRUE)
  writeLines('{"splitGraph_object": "split_spec", "schema_version": "0.3.0", "group_var": "group_id", "sample_data": ["not_an_object"]}', tf3)
  rep3 <- validate_split_spec_json(tf3)
  expect_false(rep3$valid)
  expect_true(any(grepl("sample_data\\[1\\]: must be a JSON object", rep3$issues)))
})

test_that("Defect #7 & #8: subset_graph drops excluded related subjects and undirected pairwise edges canonicalize endpoints", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P2", "P3"),
    stringsAsFactors = FALSE
  )
  pairs <- data.frame(
    id1 = c("P2", "P1", "P2"),
    id2 = c("P1", "P2", "P3"),
    kinship = c(0.25, 0.25, 0.25),
    stringsAsFactors = FALSE
  )
  rel <- relatedness_edges_from_kinship(pairs, threshold = 0.1)
  # Symmetric rows (P2, P1) and (P1, P2) collapse into 1 canonical edge.
  expect_identical(nrow(rel$data), 2L)
  g <- add_edges(graph_from_metadata(meta), rel)

  # Both directions of query_neighbors(..., direction = "out") reach undirected neighbors.
  n_p2 <- query_neighbors(g, "subject:P2", edge_types = "subject_related_to", direction = "out")
  expect_setequal(n_p2$table$node_id, c("subject:P1", "subject:P3"))

  # Subsetting to S1 only keeps P1, not excluded relative P2 or P3.
  g_sub <- subset_graph(g, samples = "S1")
  expect_identical(g_sub$nodes$data$node_id[g_sub$nodes$data$node_type == "Subject"], "subject:P1")
})

test_that("Defect #9 & #11: query_paths flags truncated when all paths exceed max_length, and detect_dependency_components covers all node/pairwise types", {
  tp <- create_nodes(data.frame(timepoint_id = paste0("T", 1:5)), "Timepoint", "timepoint_id")
  e_tp <- create_edges(
    data.frame(f = paste0("T", 1:4), t = paste0("T", 2:5)),
    "f", "t", "Timepoint", "Timepoint", "timepoint_precedes"
  )
  s_nodes <- create_nodes(data.frame(sample_id = c("S1", "S5")), "Sample", "sample_id")
  e_st <- create_edges(
    data.frame(s = c("S1", "S5"), tp = c("T1", "T5")),
    "s", "tp", "Sample", "Timepoint", "sample_collected_at_timepoint"
  )
  g_chain <- build_dependency_graph(list(s_nodes, tp), list(e_tp, e_st))

  res_trunc <- query_paths(g_chain, from = "sample:S1", to = "sample:S5", mode = "all", max_length = 3L)
  expect_identical(nrow(res_trunc$table), 0L)
  expect_true(res_trunc$metadata$truncated)

  # Site/Region/Platform are included by default in detect_dependency_components, and relatedness works in via.
  meta_site <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P2", "P3"),
    site_id    = c("NYC", "NYC", "BOS"),
    stringsAsFactors = FALSE
  )
  g_site <- add_edges(
    graph_from_metadata(meta_site),
    relatedness_edges_from_kinship(data.frame(id1 = "P2", id2 = "P3", kinship = 0.25), threshold = 0.1)
  )
  comp_default <- detect_dependency_components(g_site)
  expect_identical(
    comp_default$table$component_id[comp_default$table$sample_id == "S1"],
    comp_default$table$component_id[comp_default$table$sample_id == "S2"]
  )
  comp_rel <- detect_dependency_components(g_site, via = c("Site", "relatedness"))
  expect_identical(length(unique(comp_rel$table$component_id)), 1L)
})

# Contract additions of the 0.4.0 cycle: the stratum annotation, pairwise
# sources inside composite derivations, edge-set provenance, and the
# cross-site leakage rule.

test_that("as_split_spec fills stratum from sample-level outcomes", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P1", "P2", "P2"),
    outcome_id = c("case", "case", "ctrl", "ctrl"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  spec <- as_split_spec(derive_split_constraints(g, "subject"), graph = g)

  expect_identical(spec$stratum_var, "stratum")
  expect_identical(spec$sample_data$stratum, c("case", "case", "ctrl", "ctrl"))
  expect_true(validate_split_spec(spec)$valid)
  # Without a graph nothing can be enriched: stratum stays NA and stratum_var NULL.
  bare <- as_split_spec(derive_split_constraints(g, "subject"))
  expect_null(bare$stratum_var)
  expect_true(all(is.na(bare$sample_data$stratum)))
})

test_that("stratum falls back to subject-level outcomes and stays NA when ambiguous", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    outcome_id = c("case", "case", "ctrl"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta, outcome_scope = "subject")
  spec <- as_split_spec(derive_split_constraints(g, "subject"), graph = g)
  expect_identical(spec$sample_data$stratum, c("case", "case", "ctrl"))

  # A sample linked to two outcomes has no unique stratum.
  samples <- create_nodes(meta, "Sample", "sample_id")
  outcomes <- create_nodes(data.frame(outcome_id = c("a", "b")), "Outcome", "outcome_id")
  links <- data.frame(sample_id = c("S1", "S1", "S2"), outcome_id = c("a", "b", "a"))
  g2 <- build_dependency_graph(
    list(samples, outcomes),
    list(create_edges(links, "sample_id", "outcome_id", "Sample", "Outcome", "sample_has_outcome")),
    validate = FALSE
  )
  con <- derive_split_constraints(g2, "composite", via = "subject")
  spec2 <- as_split_spec(con, graph = g2)
  expect_identical(spec2$sample_data$stratum, c(NA, "a", NA))
  v <- validate_split_spec(spec2)
  expect_true("partial_stratum" %in% v$issues$code)
})

test_that("stratum and stratum_var round-trip through JSON and the schema validator", {
  skip_if_not_installed("jsonlite")
  meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"), outcome_id = c("a", "b"))
  g <- graph_from_metadata(meta)
  spec <- as_split_spec(derive_split_constraints(g, "subject"), graph = g)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_split_spec(spec, tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_identical(raw$stratum_var, "stratum")
  expect_identical(raw$sample_data[[1]]$stratum, "a")
  expect_true(validate_split_spec_json(tmp)$valid)
  back <- read_split_spec(tmp)
  expect_identical(back$stratum_var, "stratum")
  expect_identical(back$sample_data$stratum, spec$sample_data$stratum)
  # Provenance additions are arrays / scalars of the declared types.
  expect_type(raw$metadata$via, "list")
  expect_true(is.null(raw$metadata$threshold) || is.numeric(raw$metadata$threshold) || is.na(raw$metadata$threshold))
  expect_type(raw$metadata$igraph_version, "character")
})

test_that("composite strict accepts pairwise sources alongside direct ones", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P2", "P3", "P4"),
    batch_id   = c("B1", "B1", "B2", "B3"),
    stringsAsFactors = FALSE
  )
  pairs <- data.frame(id1 = "P2", id2 = "P3", kinship = 0.25, stringsAsFactors = FALSE)
  g <- build_dependency_graph(
    list(create_nodes(meta, "Sample", "sample_id"), create_nodes(meta, "Subject", "subject_id"), create_nodes(meta, "Batch", "batch_id")),
    list(
      create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"),
      create_edges(meta, "sample_id", "batch_id", "Sample", "Batch", "sample_processed_in_batch"),
      relatedness_edges_from_kinship(pairs, threshold = 0.1)
    )
  )

  # batch alone: {S1,S2}, {S3}, {S4}; relatedness alone: {S2,S3}; combined: {S1,S2,S3}, {S4}.
  con <- derive_split_constraints(g, "composite", via = c("batch", "relatedness"))
  gv <- grouping_vector(con)
  expect_identical(gv[["S1"]], gv[["S2"]])
  expect_identical(gv[["S2"]], gv[["S3"]])
  expect_false(gv[["S3"]] == gv[["S4"]])
  expect_identical(con$metadata$via, c("Batch", "relatedness"))
  expect_setequal(con$metadata$relations_used, c("sample_processed_in_batch", "subject_related_to"))

  # The relation name is accepted as an alias for the mode.
  con2 <- derive_split_constraints(g, "composite", via = c("Batch", "subject_related_to"))
  expect_identical(grouping_vector(con2), gv)
})

test_that("rule-based composite treats a pairwise source as a fallback", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3", "S4"),
    subject_id = c("P1", "P2", "P3", "P4"),
    batch_id   = c("B1", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  pairs <- data.frame(id1 = "P2", id2 = "P3", kinship = 0.5, stringsAsFactors = FALSE)
  g <- build_dependency_graph(
    list(create_nodes(meta, "Sample", "sample_id"), create_nodes(meta, "Subject", "subject_id"), create_nodes(meta, "Batch", "batch_id")),
    list(
      create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"),
      create_edges(meta, "sample_id", "batch_id", "Sample", "Batch", "sample_processed_in_batch", allow_missing = TRUE),
      relatedness_edges_from_kinship(pairs, threshold = 0.1)
    )
  )
  con <- derive_split_constraints(g, "composite", strategy = "rule_based",
                                  via = c("batch", "relatedness"), priority = c("batch", "relatedness"))
  sm <- con$sample_map
  expect_identical(sm$constraint_type[sm$sample_id == "S1"], "batch")
  expect_identical(sm$constraint_type[sm$sample_id == "S2"], "relatedness")
  expect_identical(sm$group_id[sm$sample_id == "S2"], sm$group_id[sm$sample_id == "S3"])
  # S4: no batch, singleton relatedness component -> unlinked.
  expect_identical(sm$constraint_type[sm$sample_id == "S4"], "unlinked")
  expect_identical(con$metadata$priority, c("batch", "relatedness"))
})

test_that("thresholds are recorded on the edge set, the graph, and the split_spec", {
  meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"), stringsAsFactors = FALSE)
  pairs <- data.frame(id1 = "P1", id2 = "P2", kinship = 0.4, stringsAsFactors = FALSE)
  rel <- relatedness_edges_from_kinship(pairs, threshold = 0.125)
  expect_identical(rel$source$threshold, 0.125)
  expect_identical(rel$source$metric, "kinship")

  coords <- data.frame(sample_id = c("S1", "S2"), x = c(0, 1), y = c(0, 0))
  sp <- spatial_edges_from_coords(coords, radius = 2)
  expect_identical(sp$source$threshold, 2)
  expect_identical(sp$source$metric, "distance")

  g <- build_dependency_graph(
    list(create_nodes(meta, "Sample", "sample_id"), create_nodes(meta, "Subject", "subject_id")),
    list(create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"), rel, sp)
  )
  expect_identical(g$metadata$edge_sources$subject_related_to$threshold, 0.125)
  expect_identical(g$metadata$edge_sources$sample_adjacent_to$threshold, 2)

  con <- derive_split_constraints(g, "relatedness")
  expect_identical(con$metadata$threshold, 0.125)
  expect_identical(con$metadata$threshold_metric, "kinship")
  spec <- as_split_spec(con, graph = g)
  expect_identical(spec$metadata$threshold, 0.125)

  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  back <- read_dependency_graph(tmp)
  expect_identical(back$metadata$edge_sources$subject_related_to$threshold, 0.125)
  expect_true(validate_graph_json(tmp)$valid)
})

test_that("a subject collected at several sites raises subject_cross_site_overlap", {
  meta <- data.frame(
    sample_id  = c("S1", "S2", "S3"),
    subject_id = c("P1", "P1", "P2"),
    site_id    = c("NYC", "BOS", "NYC"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  report <- validate_graph(g)
  hit <- report$issues[report$issues$code == "subject_cross_site_overlap", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "warning")
  expect_true(all(c("subject:P1", "sample:S1", "sample:S2", "site:NYC", "site:BOS") %in% hit$node_ids[[1]]))

  # Severed by subject or site grouping, not by batch grouping.
  risks_subject <- as.data.frame(summarize_leakage_risks(g, constraint = derive_split_constraints(g, "subject")))
  risks_site <- as.data.frame(summarize_leakage_risks(g, constraint = derive_split_constraints(g, "site")))
  expect_true(risks_subject$severed[risks_subject$category == "subject_cross_site_overlap"])
  expect_true(risks_site$severed[risks_site$category == "subject_cross_site_overlap"])
})

test_that("review fixes: overrides serialise as an object, empty edge sets add cleanly, thresholds round-trip", {
  skip_if_not_installed("jsonlite")
  g <- graph_from_metadata(data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2")))
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  write_dependency_graph(g, f)
  raw <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_true(is.list(raw$metadata$validation_overrides))
  expect_false(any(grepl('"validation_overrides": []', readLines(f), fixed = TRUE)))
  expect_true(any(grepl('"validation_overrides": {}', readLines(f), fixed = TRUE)))

  # The R-side validator rejects an array where the schema wants an object,
  # including the EMPTY array that the writer used to produce (jsonlite
  # distinguishes `{}` from `[]` by names, so the validator must too).
  write_array <- function(value) {
    raw$metadata$validation_overrides <- value
    writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null", na = "null"), f)
    validate_graph_json(f)
  }
  empty_report <- write_array(list())
  # `toJSON()` without `pretty` writes compact text, so match on the parsed
  # value rather than on spacing: an empty JSON array parses to a list with
  # NULL names, an empty object to one with `character(0)` names.
  expect_null(names(jsonlite::fromJSON(f, simplifyVector = FALSE)$metadata$validation_overrides))
  expect_false(empty_report$valid)
  expect_true(any(grepl("validation_overrides", empty_report$issues)))
  expect_false(write_array(list("x"))$valid)
  expect_error(read_dependency_graph(f, validate = TRUE), class = "splitgraph_schema_error")
  # An empty object still passes, and so does an empty attrs entry.
  expect_true(splitGraph:::.depgraph_is_json_object(jsonlite::fromJSON("{}", simplifyVector = FALSE)))
  expect_false(splitGraph:::.depgraph_is_json_object(jsonlite::fromJSON("[]", simplifyVector = FALSE)))

  # build_dependency_graph refuses an unnamed overrides list up front.
  expect_error(
    graph_from_metadata(data.frame(sample_id = "S1", subject_id = "P1"),
                        validation_overrides = list(TRUE)),
    class = "splitgraph_error"
  )

  # Nothing the package can write may fail its own validator: a hand-built
  # spec with no metadata, a spec with an explicitly empty metadata list, and
  # an edgeless graph all have empty objects/arrays in awkward places.
  bare <- tempfile(fileext = ".json")
  on.exit(unlink(bare), add = TRUE)
  write_split_spec(split_spec(), bare)
  expect_true(validate_split_spec_json(bare)$valid)
  write_split_spec(
    split_spec(sample_data = data.frame(sample_id = "S1", group_id = "g1", stringsAsFactors = FALSE),
               metadata = list()),
    bare
  )
  expect_true(validate_split_spec_json(bare)$valid)
  expect_s3_class(read_split_spec(bare, validate = TRUE), "split_spec")

  edgeless <- build_dependency_graph(
    list(create_nodes(data.frame(sample_id = c("S1", "S2")), "Sample", "sample_id")),
    list(graph_edge_set()), validate = FALSE
  )
  eg <- tempfile(fileext = ".json")
  on.exit(unlink(eg), add = TRUE)
  write_dependency_graph(edgeless, eg)
  expect_true(validate_graph_json(eg)$valid)
  expect_identical(nrow(read_dependency_graph(eg, validate = TRUE)$edges$data), 0L)

  # add_edges() with an edge set in which nothing passed the threshold
  empty <- relatedness_edges_from_kinship(data.frame(id1 = "P1", id2 = "P2", kinship = 0.01), threshold = 0.1)
  expect_identical(nrow(empty$data), 0L)
  g2 <- add_edges(g, empty)
  expect_identical(nrow(g2$edges$data), nrow(g$edges$data))
  expect_identical(g2$metadata$edge_sources$subject_related_to$threshold, 0.1)

  # threshold / threshold_metric round-trip as typed NA on non-pairwise specs
  spec <- as_split_spec(derive_split_constraints(g, "subject"), graph = g)
  sf <- tempfile(fileext = ".json")
  on.exit(unlink(sf), add = TRUE)
  write_split_spec(spec, sf)
  back <- read_split_spec(sf)
  expect_identical(back$metadata$threshold, NA_real_)
  expect_identical(back$metadata$threshold_metric, NA_character_)
  expect_true(isTRUE(all.equal(back$metadata[setdiff(names(back$metadata), "derived_at")],
                               spec$metadata[setdiff(names(spec$metadata), "derived_at")])))
})

test_that("an ambiguous sample-level outcome stays NA instead of borrowing the subject outcome", {
  meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P1"), stringsAsFactors = FALSE)
  outcomes <- create_nodes(data.frame(outcome_id = c("case", "ctrl")), "Outcome", "outcome_id")
  links <- data.frame(sample_id = c("S1", "S1"), outcome_id = c("case", "ctrl"))
  subj_out <- data.frame(subject_id = "P1", outcome_id = "case")
  g <- build_dependency_graph(
    list(create_nodes(meta, "Sample", "sample_id"), create_nodes(meta, "Subject", "subject_id"), outcomes),
    list(
      create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"),
      create_edges(links, "sample_id", "outcome_id", "Sample", "Outcome", "sample_has_outcome"),
      create_edges(subj_out, "subject_id", "outcome_id", "Subject", "Outcome", "subject_has_outcome")
    ),
    validate = FALSE
  )
  spec <- as_split_spec(derive_split_constraints(g, "subject"), graph = g)
  # S1 is ambiguous so its stratum stays NA; S2 inherits the subject outcome.
  expect_identical(spec$sample_data$stratum, c(NA, "case"))
})

test_that("invalid mode / strategy / format / focus arguments raise classed errors", {
  g <- graph_from_metadata(data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2")))
  expect_error(derive_split_constraints(g, mode = "bogus"), class = "splitgraph_error")
  expect_error(derive_split_constraints(g, mode = "composite", strategy = "loose"), class = "splitgraph_error")
  expect_error(export_graph(g, tempfile(), format = "dot"), class = "splitgraph_error")
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(plot(g, focus = "sideways"), class = "splitgraph_error")
  expect_error(graph_from_metadata(data.frame(sample_id = "S1"), outcome_scope = "cohort"), class = "splitgraph_error")
  # query traversal arguments are classed too
  expect_error(query_neighbors(g, "sample:S1", direction = "sideways"), class = "splitgraph_error")
  expect_error(query_paths(g, "sample:S1", "sample:S2", mode = "backwards"), class = "splitgraph_error")
  expect_error(query_shortest_paths(g, "sample:S1", "sample:S2", mode = "backwards"), class = "splitgraph_error")
  # partial matching still works, as with match.arg()
  expect_s3_class(derive_split_constraints(g, mode = "subj"), "split_constraint")
  expect_identical(as.data.frame(query_neighbors(g, "sample:S1", direction = "o"))$direction[[1]], "out")
})

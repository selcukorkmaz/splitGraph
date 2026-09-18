# splitGraph 0.4.0 — Roadmap

> **Implementation status (last verified 2026-09-16, version `0.4.0`).**
> Milestones M1–M5 are implemented. M6 (release) is prepared: the version is
> bumped to `0.4.0`, NEWS is dated, and the full release checklist in
> `dev/release.md` has been run locally. What remains is outside this
> repository: pushing the branch, running the pkgdown workflow once so the
> DESCRIPTION URL resolves, and the CRAN submission itself.
> Verified state after implementation: 211 tests, 0 failures, **0 skips** —
> bioLeak 0.3.8 was installed on 2026-09-16, so the contract tests now run and
> `R CMD check` is clean without `_R_CHECK_FORCE_SUGGESTS_=false`;
> covr 91.1 % (gate 90 %);
> all seven vignettes' code executes; every derivation, validation, and JSON
> output identical to CRAN 0.3.0 on regression cohorts except the intended
> additions (stratum column, cross-site rule, edge_sources, schema 0.3.0).
> Measured after M1: composite derivation with the default `via` 0.05 s at
> 5 000 and 0.19 s at 20 000 samples (was ≈ 70 s at 500); enrichment
> (`as_split_spec(graph=)`) 0.1–0.3 s at 5 000 (was 27 s); validation
> ≈ 0.7–1.4 s at 5 000 and ≈ 2–4 s at 20 000;
> build ≈ 1–1.4 s at 5 000 and ≈ 2–5 s at 20 000; graph JSON write 1.4–1.7 s
> at 5 000 and ≈ 6 s at 20 000 (jsonlite-bound). Every step is linear; the
> "< 10 s at 50 000 for the full pipeline" exit target is therefore *not* yet
> met for build + validate + write combined (≈ 30–40 s extrapolated), while
> grouping and enrichment are far inside it.
>
> Also verified on 2026-09-16, once pandoc and the optional packages were
> installed: `R CMD check --as-cran` with vignettes rebuilt (1 WARNING and
> 2 NOTEs, all environmental or version-string related, itemised in
> `dev/release.md`); `lintr::lint_package()` 0 findings after calibrating
> `.lintr` and rewrapping 8 over-long lines; the pkgdown site builds (72
> pages, all 7 articles) into `pkgdown-site/`; and a reverse-dependency run of
> bioLeak 0.3.8's own suite against this tree passes (281 tests, 0 failures).
>
> Not done in this tree and deliberately left for the maintainer: D2 (bioLeak
> release), D4 publication to PyPI (`pyproject.toml` is ready), the `styler`
> pass (styler not installed), the first pkgdown **deployment** (workflow now
> committed; the DESCRIPTION URL 404s until it runs once), and the CRAN
> submission itself (M6, see `dev/release.md`). The working tree was normalised
> to LF line endings so a tarball built on Windows is clean. An independent review of the implementation
> (2026-09-15) found no high-severity defects; its two medium findings
> (`validation_overrides` serialised as `[]`, `add_edges()` on an empty edge
> set) and four low ones were fixed the same day. A follow-up verification pass
> (2026-09-16) closed the remaining cases of the same two classes: the JSON
> validators could not tell an empty array from an empty object (so they could
> not have caught the `[]` bug themselves), `split_spec()` metadata had the
> same empty-array problem, and `direction =` / `mode =` on the query functions
> still raised bare `match.arg()` errors. Installing bioLeak 0.3.8 the same day
> then corrected the consumer table itself: composite mode fails there too, so
> the documented interim path is now `make_split_plan(group = "group_id")`.
> Gates after that pass: 211 tests, 0 failures, 0 warnings, 0 skips;
> `R CMD check` OK with every Suggests installed; coverage 91.1 %; all seven
> vignettes execute; nothing the package writes fails its own validator.

**Theme:** Make splitGraph a *mature* representation layer: fast enough for real
cohorts, complete enough that the `split_spec` contract is self-contained, and
clean enough that the 0.1-era API debt is gone. 0.3.0 broadened *what* splitGraph
can describe; 0.4.0 makes describing it dependable at scale. Nothing here produces
folds, fits models, applies purge/embargo, or audits performance — those remain
**bioLeak's** responsibility.

**Release shape:** one deliberate breaking release. Every removal and rename lands
here so that 0.4.x → 1.0 can be additive. Released as `0.4.0`.

**Starting point (2026-09-14):** 0.3.0 on CRAN; development tree has the post-release
fixes (factor identifier coercion, tolerant enrichment, JSON array shape,
`dependency_constraint` removed) with 168 tests, 0 failures, and `R CMD check`
`Status: OK` — with local caveats: it was run with `_R_CHECK_FORCE_SUGGESTS_=false`
because bioLeak is not installed here, so **four** tests skip under a plain run — the
three bioLeak contract tests (exercised only in CI) and the Python conformance test,
which skips wherever `NOT_CRAN` is unset (it passes here when run with
`NOT_CRAN=true`; see E2). Vignettes were not built because pandoc is absent; all four
vignettes' code chunks were executed separately via `knitr::purl` and pass.

---

## What "mature" means for this release (exit criteria)

| Dimension | Today | 0.4.0 target |
|---|---|---|
| Scale | composite derivation with the **default** `via` is quadratic: ≈ 70 s at only 500 samples, ≈ 3.5 min at 750; restricted to subject+batch it is ≈ 5 min at 5 000; validation ≈ 3–7 s at 5 000 depending on the run (7.3 s in the tabulated run) | full pipeline (build → validate → derive → spec → write) < 10 s at 50 000 samples; every step ≤ O(n log n) or O(n + m) |
| Contract | `split_spec` lacks a stratum column; pairwise modes cannot join a composite; reader does not validate | self-contained spec (group, block, order, **stratum**); any relation composable; opt-in schema validation on read |
| API | 5 deprecated aliases + deprecated `checks=` still exported; no graph editing; export only via JSON or by dropping to `as_igraph()` | deprecated surface removed; `subset_graph()`, `combine_graphs()`, `export_graph()`; typed error conditions |
| Consumers | one adapter (bioLeak) reading the 0.2.0 field subset and **erroring** on the six 0.3.0 modes; stale fastml mention | documented, tested consumer matrix; bioLeak reads the full contract (bioLeak-side release) |
| Quality | coverage measured only in CI; no pkgdown; no perf regression guard | coverage ≥ 90 % with local tooling; pkgdown site; benchmark script + budgeted perf test |
| Docs | 4 vignettes (the long end-to-end one doubles as the introduction); all examples synthetic | short quick-start with a mode decision table + real public-dataset case study + FAQ on "why not `make_split_plan()`" |

---

## Guardrails (unchanged)

| splitGraph *does* | splitGraph *does not* (bioLeak / fastml own) |
|---|---|
| Model dependency structure as a typed graph | Generate resamples / folds |
| Validate structure; derive split **constraints** | Stratified **splitting**, purge/embargo **execution** |
| Emit + validate the `split_spec` interchange format | Model fitting, tuning, performance auditing |
| Carry annotations (group, block, order, stratum) | rsample / tidymodels adapters |
| Cross-language handoff (R ↔ JSON ↔ Python) | Statistical leakage evidence |

A *stratum* column is an annotation (which outcome level a sample carries), not a
stratified split; it stays on this side of the line.

---

## Workstream A — Performance & scalability (P0, blocking)

Measured on the development tree (single core, Windows, R 4.5.1; synthetic cohort
with subjects ≈ n/3, batches ≈ n/50, 5 studies, 8 sites, 4 timepoints). The composite
column below is for `via = c("subject", "batch")` only:

| n | build | validate | derive subject | derive composite (subject+batch) | as_split_spec(graph=) | write graph |
|---|---|---|---|---|---|---|
| 500 | 0.2 s | 0.4 s | 0.4 s | 11.6 s | 1.7 s | 0.5 s |
| 2 000 | 0.6 s | 1.7 s | 1.6 s | 78 s | 9.5 s | 2.1 s |
| 5 000 | 1.5 s | 7.3 s | 5.4 s | **297 s** | 27 s | 7.3 s |

With the **default** `via` (subject, batch, study, time) on the same cohort the picture
is far worse, because a target shared by n/5 samples (a study) or n/4 samples (a
timepoint) yields O(n²) sample pairs:

| n | derive composite (default `via`) |
|---|---|
| 250 | 16.5 s |
| 500 | 70.4 s |
| 750 | ≈ 208 s (independent re-run) |

That is exponent ≈ 2.1: at 5 000 samples the default call would take hours, not
minutes. The subject+batch column grows ≈ n^1.4 only because those targets are small
and bounded. Validation growth is milder and varies between runs (n^1.3–1.6). Profiling
(n = 1 500 and 5 000) attributes the cost to row-at-a-time construction, not to igraph:

- **A1. Composite / component derivation.** `.depgraph_shared_dependency_table()`
  enumerates every sample pair per shared target with `utils::combn()` and builds one
  `data.frame` per pair; `.depgraph_shared_dependency_edge_ids()` then rescans the whole
  edge table once per pair (O(pairs × edges) always, not just in a worst case). At
  n = 1 500 the split was roughly 3:1 between the two (≈ 73 % / 27 %); at n = 500 an
  independent profile gave ≈ 87 % / 13 %, so treat the ratio as indicative — the two
  hotspots are the same at every n. In addition, `detect_dependency_components()` computes the shared table
  **twice** per call — once directly and once inside
  `.depgraph_project_sample_dependencies()` (confirmed by tracing). Fix: compute
  components on the **bipartite sample–target subgraph** directly
  (`igraph::components()` on Sample ∪ via-type nodes, then take the Sample
  membership) — no pairwise projection is needed for grouping. Keep the projection
  table as a result built once and vectorised (`split()` + `merge()`), not per-pair
  frames; see the M1 note on keeping `metadata$projection_edges` populated.
- **A2. Direct assignment and sample maps.** `.depgraph_direct_assignment()` and
  `.depgraph_build_sample_map()` build one `data.frame` per sample; enrichment calls the
  former seven times (six block sources plus the time path). Vectorise with `match()`
  over the edge table; construct each sample map in a single `data.frame()` call.
- **A3. Serialisation.** `write_dependency_graph()` / `write_split_spec()` build one
  list per row before `toJSON()`. Pass the data frames to jsonlite directly (its default
  `dataframe = "rows"` emits the same row objects) with `attrs` pre-converted. Watch
  one detail: an empty `attrs` entry must still serialise as `{}` (today forced via a
  named empty list), not `[]` — the schema types `attrs` as `object`, so `[]` fails any
  external JSON Schema validator. The R-side `validate_graph_json()` would *not* catch
  it, because it never inspects `attrs` (see B5), so cover this with a round-trip test.
- **A4. Pairwise helpers.** `spatial_edges_from_coords()` loops over all pairs
  (1.8 s at 1 500 points). Use `which(dmat <= radius & upper.tri(dmat), arr.ind = TRUE)`;
  for large n offer a neighbour-search backend (`RANN`/`FNN` in Suggests). Accept a
  square kinship / GRM **matrix** in `relatedness_edges_from_kinship()` (e.g. PLINK
  `--make-rel square` output) in addition to the long pair table it takes today;
  KING and GCTA emit pair tables, which the current signature already covers.
- **A5. Validation.** 7.3 s at 5 000 samples in the tabulated run, ≈ 3–7 s across repeat runs (superlinear, n^1.3–1.6), with two causes visible in
  the profile: `.validate_structural()` calls `.depgraph_edge_type_schema()` — a
  `data.frame` subset — once **per edge**, and every issue becomes a one-row
  `data.frame` that is later `rbind`-ed (2 557 issues on the benchmark cohort, mostly
  per-subject advisories). Vectorise the signature check with one join against the
  schema table, and accumulate issue columns as vectors, building the table once. (A
  suspected named-vector `[[` lookup cost was tested and is negligible.)
- **A6. Guard rails.** Add `inst/bench/pipeline.R` (reproduces the table above) and a
  `test-performance.R` with a generous wall-clock budget, skipped on CRAN, so a
  regression to quadratic behaviour fails CI rather than a user's session.

Exit: the 50 000-sample budget above; results identical to 0.3.0 on the existing test
fixtures (grouping vectors, component ids up to relabelling, JSON round-trips).

---

## Workstream B — `split_spec` contract completeness

- **B1. Stratum annotation.** Add `stratum` to `sample_data`, filled from the Outcome
  node linked by `sample_has_outcome` (or via `subject_has_outcome`) when exactly one
  outcome per sample exists; NA otherwise. Declare `stratum_var` on the spec. The
  Python reader's generic `strata(column)` already anticipates this, so scikit-learn's
  `StratifiedGroupKFold` can run from the spec alone. bioLeak's `stratify=` still reads
  the outcome from the caller's data frame, so on that side `stratum` is a consistency
  check rather than a replacement.
- **B2. Any relation composable.** Unify the projection-edge machinery so
  `mode = "composite", via = c("subject", "relatedness", "spatial", ...)` works, removing
  the limitation documented in 0.3.0.9000. Rule-based strategy gains pairwise sources
  as lowest-priority fallbacks.
- **B3. Leakage rules for the 0.3.0 relations.** `.validate_leakage()` and the
  `severed` table know nothing about Site, Region, Platform, Assay. One rule is a clear
  analogue of an existing one: `subject_cross_site_overlap` (mirrors
  `subject_cross_study_overlap`). Further structural rules (e.g. site and study being
  one-to-one, so a site holdout is silently a study holdout) should be drafted against
  the real-data case study in F2 rather than invented up front; anything that needs
  outcome distributions is statistical and belongs to bioLeak. Extend
  `.leakage_severed_by_constraint()` for whatever lands.
- **B4. Schema 0.3.0.** B1–B3 add fields → bump `.depgraph_schema_version` to
  `"0.3.0"` (same major, additive, loads silently). Publish schemas under a **versioned**
  URL path (`inst/schema/0.3.0/…`) so `$schema` references stop drifting with `main`.
  ⚠️ `test-schema-version.R` pins `"0.2.0"` and `test-json-schema.R` hard-codes it in
  eight places (plus `"0.1.0"` legacy cases that must keep loading); update those
  with the bump and add a `0.2.0 → 0.3.0` migration test, as was done for 0.1 → 0.2.
- **B5. Validating reader.** `read_split_spec(path, validate = FALSE)` /
  `read_dependency_graph(path, validate = FALSE)`: when `TRUE`, run the shipped
  structural validator (and `validate_graph()` for graphs) and error on failure.
  Document the default explicitly (already clarified in 0.3.0.9000 docs). While there,
  close the known holes in the R-side validators: neither checks `attrs` is an object,
  and `validate_split_spec_json()` checks neither the `sample_data` column types nor the
  `metadata` vector fields the schema declares.
- **B6. Provenance.** Record thresholds (`kinship_threshold`, `spatial_radius`), `via`,
  `priority`, and the `igraph` version in `metadata`. Note the thresholds are applied
  when the edge set is built and are *not* known at derivation time today (only the
  per-edge metric survives), so the edge-building helpers must first store them on the
  `graph_edge_set$source` list and the graph must carry that through. Make the
  vector-valued metadata fields (already forced to arrays) explicit in the schema.

---

## Workstream C — API maturity and cleanup (breaking, bundled)

- **C1. Remove the 0.1-era surface.** `new_depgraph()`, `new_depgraph_nodes()`,
  `new_depgraph_edges()`, `build_depgraph()`, `validate_depgraph()`, and the `checks=`
  argument of `validate_graph()` have been deprecated since 0.2.0. Remove them; keep a
  NEWS migration table.
- **C2. Graph editing.** `subset_graph(g, samples = )` (keep the induced structure for a
  sample subset; the composite-strict path already recomputes components within a
  subset, but nothing exposes the subgraph itself), `combine_graphs(g1, g2)` (union with
  id-collision checks), and `add_edges(g, edge_set)` returning a re-validated graph.
  Today users must rebuild from node/edge sets.
- **C3. Export.** `export_graph(g, file, format = c("graphml", "gml", "nodes_csv",
  "edges_csv"))` as promised in the v1 blueprint, so graphs open in Cytoscape / Gephi.
  This is a thin convenience over the already-exported `as_igraph()` +
  `igraph::write_graph()`; its real work is that GraphML attributes must be scalars, so
  the `attrs` list-column has to be flattened to typed columns (or dropped with a
  message) on export.
- **C4. Typed conditions.** Replace bare `stop()` with classed conditions
  (`splitgraph_error`, subclasses `splitgraph_schema_error`,
  `splitgraph_reference_error`, `splitgraph_ambiguity_error`) carrying the same `code`
  vocabulary as validation issues, so callers can `tryCatch()` on class and the
  Python side can map codes.
- **C5. Input adapters (Suggests only).** `graph_from_metadata()` as an S3 generic with
  a `SummarizedExperiment` method reading `colData()`; a documented long-format path for
  multi-assay samples (one row per sample × assay).
- **C6. Plot focus.** `plot(g, focus = c("full", "sample_projection", "ego"), node =)`
  per the blueprint's `visualize_graph()`. All 11 node types already have palette
  entries, so the `_other_` colour is reachable only for hand-built graphs carrying an
  unknown type; keep it as a defensive default.
- **C7. Consistency sweep.** Identical column order across all emitted frames, and the
  `NA`-column conventions of `sample_data` documented once in `?split_spec`. The local
  `%||%` is NA-aware and deliberately differs from base R's (≥ 4.4); keep it, but say so
  in a comment so nobody "simplifies" it away.

---

## Workstream D — Consumer seam (the deferred gap)

Recorded 2026-09-14 (corrected after independent review): bioLeak 0.3.8's
`as_leaksplits()` forwards only `group_id`, `batch_group`, `study_group`,
`timepoint_id`, `order_rank`, and its mode map covers only subject / batch / study /
time / composite. For any other `constraint_mode` (site, region, platform, assay,
relatedness, spatial) the lookup `mode_map[[src_mode]]` on a named atomic vector
**errors** with "subscript out of bounds" — the intended `subject_grouped` fallback on
the next line is unreachable. So a 0.3.0-mode spec does not degrade silently; it
cannot be consumed by bioLeak at all today. `make_split_plan()` also has no generic
blocking axis, so the full fix is a bioLeak feature. Local `bioLeak` (0.3.5) and
`fastml` (0.7.8) repos lag CRAN (0.3.8, 0.7.10).

Corrected again 2026-09-16, after installing bioLeak 0.3.8 and actually running
the seam rather than reading it: the adapter is worse than the static reading
suggested. **`composite` also fails**, in both strategies — it *is* in the mode
map, but maps to `make_split_plan(mode = "combined")` without the
`constraints` / `primary_axis` that mode requires, so it errors with
`'primary_axis' must be a list with 'type' and 'col' elements`. Verified
acceptance is exactly subject / batch / study / time. The interim path that
does work for every other mode is to join `group_id` onto the observation frame
and call `make_split_plan(mode = "subject_grouped", group = "group_id")`
directly; D1 now documents and tests that.

- **D1 (splitGraph, this release).** Remove the fastml consumer mention in
  `R/split-spec.R`; extend `test-bioleak-contract.R` to *pin today's behaviour* (the
  four supported modes round-trip; the six 0.3.0 modes and both composite
  strategies raise bioLeak's error — `expect_error()`s that will start failing,
  deliberately, when D2 ships; and the `group_id` workaround succeeds); document in
  `?as_split_spec` and the README exactly which modes and columns the reference consumer
  accepts, and suggest `mode = "subject"`/`"composite"` or a manual `group_id` handoff
  as the interim path for the new relations.
- **D2 (bioLeak, separate release, after syncing the repo to 0.3.8).** Three
  defects to fix, in order: (a) `composite` is mapped but unusable — supply
  `constraints` (or `primary_axis` / `secondary_axis`) from the spec's
  `metadata$via` when `mode = "combined"`; (b) make the unknown-mode fallback
  reachable (`mode_map[src_mode]` / `match()` / `switch`) and map the six new
  modes explicitly; (c) forward the four block columns and `stratum`, and
  accept a generic `block=` axis in `make_split_plan()`. Then flip the
  splitGraph contract test from "expects error" to "asserts blocking is
  honoured".
- **D3 (rsample).** Turn the illustrative `group_vfold_cv()` adapter in the cookbook into
  an *executed* example under `Suggests: rsample`. No new export — the boundary holds.
- **D4 (Python).** Publish `splitspec` to PyPI as a stdlib-only package with the
  conformance script as its test suite; pin the schema major it supports.

---

## Workstream E — Quality infrastructure

- **E1. Coverage.** Add `covr` to the dev tooling and a `dev/coverage.R` entry; set a
  Codecov threshold (fail under 90 %). Measure before targeting: the obvious candidates
  (edge dedupe conflicts, partial time ordering, plot layouts) turned out to be covered
  already, so the real gaps are only knowable from the report.
- **E2. CI.** Two facts verified 2026-09-14: `setup-r-dependencies` installs Suggests
  by default, so the bioLeak contract test already runs in `R-CMD-check` (it gates on
  `skip_if_not_installed()`); but neither `check-r-package` nor `rcmdcheck` sets
  `NOT_CRAN`, so the Python conformance test — which gates on `skip_on_cran()` — has
  **never run in CI**. Fix: set `NOT_CRAN: true` in the workflow `env` (Ubuntu runners
  ship `python3`), add the `perf-budget` job running `test-performance.R` under the
  same flag, and pin the bioLeak version used by the contract test in the job log so a
  CRAN bioLeak release changing the seam is visible.
- **E3. pkgdown.** `_pkgdown.yml` with a grouped reference index (Build · Validate ·
  Query · Derive · Interchange · Pairwise), deployed via Actions; link from README.
- **E4. Static checks and line endings.** `lintr` config committed; `styler` pass once
  (bundled with the breaking release so the diff noise lands in one commit).
  `.gitattributes` (`* text=auto`) normalises only the git *index*: on this Windows
  checkout 59 tracked files are CRLF in the worktree, and a tarball built here carries
  CRLF in most text files (the CRAN 0.3.0 tarball is LF only because it was built on
  another machine). `R CMD check` does not flag this. Fix: build release tarballs in CI
  (Linux) or set `core.autocrlf=false` + `git add --renormalize .` on the release
  machine, and add a tarball line-ending check to the release checklist (E5).
- **E5. Release checklist.** `dev/release.md`: `rhub` multi-platform check, `revdepcheck`
  against bioLeak, `urlchecker`, NEWS review, schema version review, `CITATION` bump.

---

## Workstream F — Documentation

- **F1. Quick start.** The existing `leakage-aware-workflow` vignette already covers the
  end-to-end path (it shows the fast path briefly, then does the main walkthrough via
  the explicit constructors) but is over 1 100 lines. Add a short
  quick-start (metadata frame → JSON `split_spec` via the fast path) with the decision
  table "which mode for which structure", and make it the first entry in the index.
- **F2. Real-data case study** vignette: a public multi-batch, multi-site cohort (e.g. a
  GEO series with repeated subjects), cached as `inst/extdata`, showing how validation
  catches an actual provenance problem and how the derived groups differ between
  subject, composite-strict, and rule-based modes.
- **F3. FAQ / design notes**: "Why not `make_split_plan(group=, batch=)` directly?",
  "When does composite-strict over-merge?", "How do thresholds interact with transitive
  closure?", schema versioning policy in one place.
- **F4. Reference hygiene**: every exported function has a runnable example; every
  `sample_data` column documented once with type and NA semantics; node/edge type
  cheat-sheet as a table in `?splitGraph`. Fix an existing over-claim while there: the
  README's scope table (line 94) already lists "Carrying stratum … annotations" as a
  present capability although no stratum column exists until B1 lands — either ship
  B1 first or reword the README.
- **F5. Paper alignment**: update `paper.md` claims to match D1 (state exactly what the
  reference consumer reads) and cite the performance envelope from A.

---

## Milestones / sequencing

1. **M1 — Performance (A).** First, because every later workstream adds fields and
   tests that would otherwise inherit the superlinear cost. Land with the benchmark and
   the budgeted perf test. *Can ship as 0.3.1 if a CRAN patch is wanted early, provided
   it stays non-breaking: `detect_dependency_components()` must keep returning
   `metadata$projection_edges` and the selected `edges` table, and constraint
   `sample_map` columns must not change.*
2. **M2 — Breaking cleanup (C1, C4, C7).** Remove deprecated surface, introduce typed
   conditions, consistency sweep. Bundle the `styler` pass here.
3. **M3 — Contract (B, C2, C3, C6).** Stratum, composable pairwise, new leakage rules,
   schema 0.3.0 + versioned URLs, validating reader, graph editing, export, and plot
   focus (which reuses the projection machinery from A1/B2).
4. **M4 — Seam and interop (D1, D3, D4, C5).** Consumer table, pinned contract test,
   executed rsample example, PyPI reader, SummarizedExperiment input.
5. **M5 — Quality and docs (E, F).** Coverage gate, CI jobs, pkgdown, vignettes, FAQ,
   paper alignment.
6. **M6 — Release.** Checklist in E5; coordinate the bioLeak-side D2 release so the
   contract test can be tightened in 0.4.1 rather than blocking 0.4.0.

---

## Risks / watch-items

- **Behaviour drift while vectorising.** Component *labels* may change even when the
  partition is identical; tests must compare partitions (e.g. via
  `igraph::compare(method = "nmi")` or canonical relabelling), not raw `component_k`
  strings. Pin the projection table's row order explicitly.
- **Schema bump churn.** B1–B3 are additive (same major); resist any rename. If a field
  must change meaning, that is a 1.0 conversation, not 0.4.0.
- **Breaking-change blast radius.** bioLeak is the only known reverse dependency (CRAN
  reverse Suggests). Its 0.3.8 sources call **no** splitGraph function at all — the one
  mention of `splitGraph::as_split_spec` is inside an error-message string — and read
  four `split_spec` fields (`sample_data`, `group_var`, `constraint_mode`, `time_var`)
  plus the `sample_id`, `batch_group`, `study_group`, `timepoint_id`, `order_rank`
  columns (verified 2026-09-14). C1 is therefore safe, and those names are the ones
  that must never change without a coordinated bioLeak release; re-verify against the
  bioLeak version current at release.
- **Scope creep from the blueprint's §12.** Gene / Drug / Pathway knowledge-graph
  extensions stay out; 0.4.0 is about maturity of the dataset-structure layer.
- **Stratum ≠ stratification.** Keep B1 as an annotation; never balance folds here.
- **Windows tooling.** `python3` on Windows may be a Store stub (handled in tests);
  document `python` fallback for contributors.

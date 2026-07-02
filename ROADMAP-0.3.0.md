# splitGraph 0.3.0 — Roadmap

**Theme:** Deepen splitGraph as the *cross-language, validatable representation layer*
for dataset leakage structure — richer typed structure in, a hardened tool-agnostic
`split_spec` IR out. Everything here describes structure or derives **constraints**;
nothing here generates folds, fits models, stratifies splits, applies purge/embargo,
or audits performance. Those remain **bioLeak's** responsibility (splitGraph feeds it).

**Release shape:** larger release (publication-enabling IR work *and* broader
structural features). Set `DESCRIPTION` to `0.2.0.9000` now; release as `0.3.0`.

---

## Guardrails (the bioLeak boundary)

| splitGraph 0.3.0 *does* | splitGraph 0.3.0 *does not* (bioLeak owns) |
|---|---|
| Model dependency structure as a typed graph | Generate resamples / folds (`make_split_plan`) |
| Validate structure; derive split **constraints** | Stratified **splitting**, purge/embargo **execution** |
| Emit + validate the tool-agnostic `split_spec` IR | Model fitting, tuning, performance auditing |
| Carry annotations (stratum, ordering, blocking) | rsample/tidymodels adapters (`as_rsample`, `as_leaksplits`) |
| Cross-language handoff (R ↔ JSON ↔ Python) | Statistical leakage evidence (ΔLSI, permutation gaps) |

Rule of thumb: if a feature *describes structure or derives a constraint*, it belongs
here; if it *produces folds, fits, or measures performance*, it belongs in bioLeak.

---

## Workstream A — Structural modeling: four new leakage relations

Today's node types: Sample, Subject, Batch, Study, Timepoint, Assay, FeatureSet,
Outcome. Constraint modes: subject, batch, study, time, composite.

### A1. Cluster-style relations (direct grouping)
New typed nodes + sample/subject-rooted edges + auto-detected canonical columns +
validation rules + new `derive_split_constraints()` modes.

| Relation | New node | Canonical edge | Auto column | New mode |
|---|---|---|---|---|
| Site / center | `Site` | `sample_collected_at_site` (or `subject_enrolled_at_site`) | `site_id` | `"site"` |
| Assay / platform | `Platform` (+ existing `Assay`) | `sample_run_on_platform`, `assay_uses_platform` | `platform_id` / `assay_platform` | `"platform"`, `"assay"` |
| Tissue region (area) | `Region` | `sample_located_in_region` | `region_id` | `"region"` |

Note: `Assay` already exists as a node and is auto-built; A1 adds the missing
**constraint mode** for it plus platform granularity.

### A2. Pairwise (thresholded) relations — the genuinely novel piece
Some leakage sources are pairwise and continuous, not clean groups. Model them as
**undirected, thresholded edges**, then reuse the existing composite-strict
connected-component machinery to form groups (transitive closure above threshold).

| Relation | Edge (undirected) | Attr | Input helper | Derived mode |
|---|---|---|---|---|
| Genetic relatedness | `subject_related_to` | kinship / degree | `relatedness_edges_from_kinship(pairs, threshold)` | `"relatedness"` |
| Spatial proximity | `sample_adjacent_to` | distance | `spatial_edges_from_coords(coords, radius)` | `"spatial"` |

`mode = "relatedness"` / `"spatial"` derive groups as connected components over the
thresholded edge set (reusing `.derive_composite_strict_constraints` internals). This
is something `make_split_plan`'s column-based grouping structurally cannot do, and is
a strong standalone-contribution differentiator.

### A3. Plumbing for A1–A2
- Extend `.depgraph_auto_specs` + `graph_from_metadata()` auto-detection.
- Extend node/edge schema, `validate_graph()` semantic + structural rules, and the
  layered `plot()` (colors + legend) for new types.
- Allow all new modes inside `mode = "composite"` (`via = c("Subject","Site",...)`).
- `print()/summary()/as.data.frame()` coverage for new types.

---

## Workstream B — `split_spec` / graph IR hardening (publication linchpin)

- **Formal JSON Schema** (Draft 2020-12) for `dependency_graph` and `split_spec`,
  shipped in `inst/schema/` and referenced from the JSON (`$schema`).
- **Schema validator**: `validate_graph_json()` / extend `read_*()` to check files
  against the shipped schema (today they only compare `schema_version` strings).
- **Richer provenance** in `split_spec$metadata`: derivation lineage (mode, strategy,
  relations used, thresholds), splitGraph version, and per-sample constraint
  explanation already present — formalize and document as part of the contract.
- **Schema-version policy**: bump `.depgraph_schema_version` (currently `"0.1.0"`),
  document the migration/compat rule, and provide a one-shot upgrader for older JSON.
  ⚠️ `test-schema-version.R` pins `"0.1.0"` — update tests + add a migration test.

---

## Workstream C — Cross-language interoperability (the differentiator)

- **Python reference consumer** in `inst/python/splitspec/`: read `split_spec` JSON →
  `pandas` frame and yield iterators compatible with scikit-learn
  `GroupKFold` / `StratifiedGroupKFold` / `TimeSeriesSplit` (keyed on `group_id`,
  stratum annotation, and `order_rank`). Pure-Python, stdlib + pandas only.
- **Conformance tests**: R writes a `split_spec`; Python reads it and asserts the
  grouping/order matches `grouping_vector()` / `order_rank` from R. Wire as an
  optional CI job (skipped when Python absent).
- **Vignette** `cross-language-handoff.Rmd`: end-to-end R → JSON → Python → sklearn,
  proving `split_spec` is an interchange format, not bioLeak plumbing.

---

## Workstream D — Boundary governance

- **Contract test** (`Suggests: bioLeak`, skipped if absent): assert a splitGraph
  `split_spec` satisfies `bioLeak::as_leaksplits()` expectations — pins the seam so
  neither side can silently break it.
- **README "Scope & relationship to bioLeak"** section: one layer above bioLeak;
  emits `split_spec`; zero resampling/modeling deps; explicit non-goals.
- Reaffirm non-goals in `?splitGraph` package doc.

---

## Workstream E — Publication & quality scaffolding

- GitHub Actions `R-CMD-check` (multi-OS) + test-coverage reporting/badge.
- New vignette: "Modeling site, platform, relatedness, and spatial structure."
- Draft `paper.md` / statement of need framed on the **representation + IR**
  contribution (de-emphasize grouping behavior bioLeak already shows). Target
  JOSS or R Journal — **not** JSS — and disclose the bioLeak JSS submission.
- Cross-citation: splitGraph cites bioLeak as a consumer; seek a bioLeak revision
  that acknowledges splitGraph as the optional structural front-end.

---

## Milestones / sequencing

1. **M1 — Structure (A1, A3):** Site, Platform/Assay mode, Region; schema, validation,
   auto-detect, plot, new cluster modes. *Most self-contained; do first.*
2. **M2 — Pairwise (A2):** relatedness + spatial edges, threshold helpers, component
   derivation modes. *Reuses composite-strict internals.*
3. **M3 — IR hardening (B):** JSON Schema, validator, provenance, schema_version bump
   + migration. *Coordinate the version bump with M1/M2 field additions.*
4. **M4 — Interop (C):** Python reader, conformance tests, cross-language vignette.
5. **M5 — Governance + scaffolding (D, E):** contract test, README scope, CI/coverage,
   paper.md.

---

## Risks / watch-items

- **Schema bump churn:** adding fields (new relations, provenance) forces a
  `schema_version` change; gate with the migration upgrader + updated round-trip tests.
- **Scope discipline:** pairwise *thresholds* are derivation inputs, not modeling —
  keep them as constraint parameters; never compute folds.
- **Publication framing:** even in a "both" release, the paper must lead on the IR /
  representation contribution, with the Python demo doing the heavy lifting against the
  "why not just `make_split_plan`?" objection.
- **Backward compatibility:** new node types must not change default validation
  outcomes for existing graphs (additive only).

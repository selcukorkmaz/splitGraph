---
title: 'splitGraph: A validatable, cross-language representation of dataset dependency structure for leakage-aware evaluation'
tags:
  - R
  - machine learning
  - data leakage
  - cross-validation
  - reproducibility
  - bioinformatics
authors:
  - name: Selcuk Korkmaz
    orcid: 0000-0003-4632-6850
    affiliation: 1
affiliations:
  - name: Department of Biostatistics, Trakya University, Edirne, Turkey
    index: 1
date: 2 July 2026
bibliography: paper.bib
---

# Summary

Machine-learning evaluations on biomedical data are frequently optimistic
because the resampling scheme ignores the dependency structure of the data:
repeated measurements of the same subject, samples processed in the same batch,
observations from the same study, site, or platform, or genetically related
individuals are split across training and test folds, leaking information and
inflating performance [@kaufman2012leakage; @roberts2017crossvalidation].
Avoiding this requires knowing, explicitly, which samples are *not* independent.
That knowledge usually lives implicitly in metadata columns and tribal
knowledge, not in an inspectable, checkable artifact.

`splitGraph` is an R package that makes dataset dependency structure a
first-class, typed object. It represents samples and their provenance
(subjects, batches, studies, timepoints, assays, platforms, sites, anatomical
regions, and pairwise relations such as genetic relatedness and spatial
proximity) as a typed dependency graph; validates that structure; derives
deterministic split **constraints** from it; and emits a stable, tool-agnostic
split specification (`split_spec`). The `split_spec` is a documented interchange
format with a formal JSON Schema and a reference Python consumer, so the same
leakage-aware partition can be reproduced across R, JSON, and Python
(scikit-learn) without re-deriving it.

# Statement of need

Existing tooling addresses adjacent but distinct problems. Resampling libraries
such as `rsample` [@rsample] and scikit-learn's `model_selection`
[@pedregosa2011scikit] can *execute* grouped, stratified, or time-series splits
once the user supplies a grouping vector, but they do not model where that
grouping comes from, validate it, or make it portable. The representation gap —
turning heterogeneous, often inconsistent metadata into a single validated,
shareable description of "what must not be split" — is unaddressed.

`splitGraph` targets exactly this representation-and-interchange layer, and
draws a deliberate boundary: it derives constraints and emits `split_spec`, but
never generates folds, fits models, applies purge/embargo, or produces
statistical leakage evidence. Those downstream concerns are owned by consumer
tools. The reference consumer is `bioLeak`, which turns a `split_spec` into an
executable, leakage-audited split plan and provides statistical leakage
diagnostics; a `bioLeak` methods paper is under separate review at the *Journal
of Statistical Software*. Because `split_spec` is neutral, other consumers — an
`rsample` adapter, or the Python reader shipped with `splitGraph` that drives
`GroupKFold`, `StratifiedGroupKFold`, and `TimeSeriesSplit` — can use it
equally. A conformance test asserts that the Python reader recovers exactly the
grouping and ordering that R emitted, and a contract test pins the seam to
`bioLeak`.

The novel contributions relative to column-based grouping are: (1) a typed,
extensible schema of leakage relations, including *pairwise, thresholded*
relations (relatedness, spatial proximity) whose groups are formed by transitive
closure over a similarity graph — a grouping that a single categorical column
cannot express; (2) structural, semantic, and leakage-relevant validation of the
dependency structure before any split is derived; and (3) a versioned,
schema-checked, cross-language interchange format that decouples *deciding* a
leakage-aware partition from *executing* it.

# Acknowledgements

We thank users of the `bioLeak` project for feedback that shaped the
`split_spec` contract.

# References

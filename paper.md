---
title: 'splitGraph: A validatable, portable representation of dataset dependency structure for leakage-aware evaluation'
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
  - index: 1
    name: Department of Biostatistics, Trakya University, Edirne, Turkey
date: 17 September 2026
bibliography: paper.bib
---

# Summary

A predictive model is only as trustworthy as the split used to evaluate it. When
the same subject, batch, study, or sequencing run contributes rows to both the
training and the test set, the test set is no longer independent, and reported
accuracy is optimistic. Leakage of this kind has been formalised for two
decades [@kaufman2012leakage], yet it remains pervasive: a survey across
seventeen scientific fields found it affecting 294 papers, in some cases
reversing their conclusions [@kapoor2023leakage]. Avoiding it requires stating,
explicitly and before any model is fitted, which samples are *not* independent
of one another.

`splitGraph` makes that statement a first-class object. It reads an ordinary
sample-level metadata table and builds a **typed dependency graph** in which
samples, subjects, batches, studies, timepoints, assays, feature sets, sites,
anatomical regions and platforms are distinct node types connected by named
relations. It then validates that structure, derives a deterministic split
**constraint** from it, and writes a tool-agnostic **split specification**
(`split_spec`) describing which samples must travel together, which coarser axes
should be blocked, in what order samples may be evaluated, and what outcome
level each carries. The specification is a versioned JSON document with a formal
schema, so the same leakage-aware partition can be reproduced in R, in Python,
or in any other language without being re-derived — and without the reasoning
behind it being lost.

Crucially, `splitGraph` never creates folds. It decides *what must not be
split*; executing that decision is left to resampling engines.

# Statement of need

Researchers fitting models to biomedical, ecological, or otherwise structured
data are routinely told to "group by subject" or "block by batch"
[@roberts2017crossvalidation; @whalen2022pitfalls]. The advice is sound, but the
knowledge it depends on — which of thirty metadata columns encode dependence,
whether a subject appears in two studies, whether a feature set was fitted on
the whole cohort — usually lives in a spreadsheet and a collaborator's memory.
It is transcribed into a grouping vector by hand, once.

Three consequences follow. The grouping cannot be *validated*: nothing detects
that a sample was assigned to two subjects, or that a declared time ordering
contradicts the recorded timepoint sequence. It cannot be *transported*: a
collaborator in Python re-derives it from the same messy metadata and may not
reproduce it. And it cannot be *audited*: a reviewer sees the folds, not the
reasoning.

`splitGraph` addresses this representation-and-interchange layer. It is aimed at
analysts who need a leakage-aware split to be inspectable rather than merely
computed, and at package authors who want to consume a dependency-aware
partition without reimplementing its derivation.

# State of the field

Resampling infrastructure is mature. `rsample` [@rsample] in R and
scikit-learn's `model_selection` module [@pedregosa2011scikit] in Python execute
grouped, stratified and time-ordered splits competently, but each begins where
`splitGraph` ends: the user supplies a grouping vector, and its provenance,
correctness and portability are outside the library's concern.

A second family goes further for one class of dependence. `mlr3spatiotempcv`
[@schratz2024mlr3spatiotempcv] collects spatiotemporal resampling methods behind
a common interface; `blockCV` [@valavi2019blockcv] generates spatially and
environmentally separated folds; `CAST` [@meyer2018cast] adds target-oriented
validation for spatio-temporal models. These are sophisticated and, within their
domain, more capable than anything `splitGraph` offers — they model
autocorrelation as a continuous field, which `splitGraph` does not attempt.

The distinction is one of layer rather than quality. Those packages produce
*resamplings*, tied to one dependence family and one modelling framework, not a
validated serialisable description of the dependency structure itself, readable
outside the host framework. That is the gap: a researcher whose cohort has
repeated subjects *and* shared batches *and* genetically related donors has no
single object that states all of it, checks it for contradictions, and travels
to a collaborator's scikit-learn pipeline unchanged.

Contributing this upstream to an existing package was considered and rejected.
The contribution is framework-agnostic by construction; placing it inside
`mlr3`, `tidymodels` or scikit-learn would tie a portable artifact to one
ecosystem and defeat the property that motivates it. `splitGraph` is therefore
built to be *consumed* by those packages rather than to compete with them, and
ships adapters demonstrating exactly that.

# Software design

\autoref{fig:pipeline} shows the resulting data flow. Three design decisions
carry most of the weight.

![Data flow through splitGraph. A metadata table becomes a typed dependency
graph, which is validated and reduced to a split constraint, a split
specification and a schema-versioned JSON artifact. Everything right of the
dashed line belongs to a consumer.\label{fig:pipeline}](paper-figures/paper-pipeline.png)

**Typing the graph rather than generalising it.** Nodes and edges are drawn from
a closed schema of 11 node types and 17 relations, not from arbitrary strings.
This buys validation: because the schema knows that a sample has at most one
subject and that `timepoint_precedes` is acyclic, checks can run automatically
over any graph before a split is derived. An open schema would have been more
flexible and would have made those guarantees impossible.

**Separating the decision from its execution.** The `split_spec` is the
deliverable, and it is deliberately inert: a sample table plus scalar
declarations of which column plays which role. A consumer keys on the declared
roles rather than hard-coded column names, so the same file drives tools that
have never heard of each other. The format carries a JSON Schema and a
major-version compatibility policy; a reader accepts any file sharing its major
version, so a minor addition — the outcome-level annotation added in schema
0.3.0, say — leaves older files readable and older readers working.

**Choosing algorithms that survive real cohort sizes.** Dependency grouping is
computed as connected components over a bipartite sample–target graph rather
than by enumerating sample pairs, which keeps the pipeline linear in samples and
edges. On a synthetic cohort of 20,000 samples with repeated subjects, 400
batches, five studies, eight sites and four timepoints, building the graph,
validating it, deriving a composite constraint and writing the specification
take about six seconds in total on a laptop. The package depends only on
`igraph` [@csardi2006igraph] and base R, so it installs anywhere its consumers
do.

# Core functionality

**Validation** runs in three layers. *Structural* checks reject a malformed
graph (dangling edges, duplicate identifiers, an unsupported relation);
*semantic* checks reject a contradictory one (a sample assigned to two subjects,
a time index that disagrees with the recorded precedence); *leakage* checks are
advisory and describe rather than prescribe — repeated subjects, a subject
spanning several studies or sites, a feature set fitted on the whole cohort.

**Derivation** offers eleven constraint modes. Eight are *direct*: each sample
carries one grouping node (subject, batch, study, time, site, region, platform,
assay). Two are *pairwise and thresholded* — genetic relatedness and spatial
proximity — where groups form by transitive closure over a similarity graph, a
partition no categorical column can express. The eleventh, *composite*, combines
any of the others, strictly or by priority order.

**Handoff** attaches what the constraint did not use as the primary grouping:
coarser axes as blocking annotations, an ordering rank when the graph carries
time, and each sample's outcome level as a stratum annotation that `splitGraph`
records but never acts on.

# Example workflow

Given a data frame `meta` with one row per sample, the whole pipeline is five
calls:

```r
library(splitGraph)

g <- graph_from_metadata(meta)
validate_graph(g)

constraint <- derive_split_constraints(g, mode = "subject")
spec       <- as_split_spec(constraint, graph = g)
write_split_spec(spec, "split_spec.json")
```

`grouping_vector(constraint)` returns the group per sample for any R resampler.
The JSON file is what crosses the language boundary, with `X` the design
matrix:

```python
from splitspec import load_split_spec
from sklearn.model_selection import StratifiedGroupKFold

spec = load_split_spec("split_spec.json")
StratifiedGroupKFold(n_splits=5).split(X, y=spec.strata(), groups=spec.groups())
```

Nothing about the partition is recomputed in Python; it is read.

# Research impact statement

`splitGraph` has been available on CRAN since July 2026 [@splitgraph] and is
accompanied by seven vignettes and a suite of 892 test expectations, run on
every change across five continuous-integration configurations spanning macOS,
Windows and Linux, and separately measured at 91.4% statement coverage.

Its near-term significance rests on being consumed rather than merely published,
and two integrations exist and are pinned by tests. `bioLeak` — an R package for
leakage-audited evaluation, described in a manuscript under separate review —
reads a `split_spec` and turns it into an executable split plan; a contract test
asserts, against the installed `bioLeak`, exactly which fields and modes that
seam supports. The shipped Python consumer is covered by a conformance test
asserting that it recovers precisely the grouping, ordering and stratum
annotation R emitted, so the two implementations cannot drift apart between
releases.

The design has been exercised on real data: a worked case study takes the
134-sample, 20-donor, seven-cell-population cohort of GEO series GSE60424
[@linsley2014gse60424] from raw metadata to a handed-off specification, and
shows that a naive five-fold assignment would place all twenty donors on both
sides of a split.

# AI usage disclosure

**Tools.** Anthropic Claude (Claude Opus 5, and earlier Claude models during
2026), used through the Claude Code command-line interface.

**Where used.** Package source code, the test suite, the reference
documentation and vignettes, and the text of this manuscript.

**Nature and scope of assistance.** Refactoring the derivation routines to
linear-time algorithms; extending the node and edge type schema; scaffolding
and drafting test cases; drafting and revising documentation, vignettes and
this paper; and copy-editing throughout. Assistance was iterative rather than
wholesale: the model proposed implementations and prose, which were then
accepted, altered or rejected.

**Confirmation of review.** The author reviewed, edited and validated all
AI-assisted output, and made the core design decisions himself — the scope
boundary (deriving constraints but never generating folds), the typed closed
schema, the decision to make `split_spec` a versioned interchange artifact
rather than an in-memory object, and the choice to keep the package free of
resampling dependencies. The author takes full responsibility for the accuracy,
originality and licensing of the software and the paper.

**Independent verification.** Correctness was checked by mechanisms independent
of the drafting process: 892 test expectations and `R CMD check --as-cran`; a
conformance test comparing the R and Python implementations on the same
artifact; a contract test against the installed downstream consumer; and, before
release, an equivalence check against the previous CRAN version on regression
cohorts. Every empirical claim in this paper was measured, not estimated.

# Conflicts of interest and funding

The author develops and maintains `bioLeak`, the R package described above as
the reference consumer of `split_spec`; the two projects are related by design
and are cited here as such. The author declares no financial conflicts of
interest.

The work received no external financial support.

# Acknowledgements

We thank users of `bioLeak` for feedback that shaped the `split_spec` contract.

# References

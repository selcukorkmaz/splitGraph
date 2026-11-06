# cran-comments.md — splitGraph 0.5.0

This is an update of an existing CRAN package (0.4.0, published 2026-09-18).

## Resubmission

This is a resubmission of 0.5.0. Previous incoming pre-tests on
`r-devel-linux-x86_64-debian-gcc` and `r-devel-windows-x86_64` failed with 1
ERROR in `tests/testthat/test-bioleak-contract.R`:

* The first submission (`test-bioleak-contract.R:120`) unconditionally tested
  extended constraint modes against `bioLeak::as_leaksplits()`, which requires
  the upcoming `bioLeak` release rather than the CRAN-released `bioLeak 0.3.8`.
* The second submission (`test-bioleak-contract.R:175`) added a feature guard
  for `bioLeak 0.3.8`, but in the `else` branch asserted an error regex
  (`"provide a non-empty 'constraints' list"`) that did not match `bioLeak
  0.3.8`'s actual error message (`"'primary_axis' must be a list with 'type' and
  'col' elements."`).

Both are fixed: `test-bioleak-contract.R` now checks the `bioLeak 0.3.8` branch
without the mismatched regex and has been verified directly against the
CRAN-released `bioLeak 0.3.8`.

## Test environments

* Local: Windows 11 x64, R 4.5.1 (2025-06-13 ucrt), igraph 2.2.1 —
  `R CMD check --as-cran` on the built tarball: 1 WARNING, 3 NOTEs, all local to
  that machine (see below).
* win-builder r-devel and r-release, and r-devel-linux-x86_64-debian-gcc, via
  CRAN's incoming pre-test for the previous submission.

## R CMD check results

The local run reports 0 errors | 1 warning | 3 notes. All four are caused by
tooling absent from the checking machine rather than by the package, and none
appeared on CRAN's own systems:

* WARNING `'qpdf' is needed for checks on size reduction of PDFs` — qpdf is not
  installed locally.
* NOTE `unable to verify current time` — the checking machine could not reach a
  time server.
* NOTE `Skipping checking HTML validation: no command 'tidy' found` — HTML Tidy
  is not installed locally.
* NOTE `detritus in the temp directory: 'lastMiKTeXException'` — a file left by
  MiKTeX, unrelated to this package.

`checking CRAN incoming feasibility` passes locally, and
`urlchecker::url_check()` reports all URLs in the package as valid.

A spell check (`spelling::spell_check_package()`) flags exactly one word in
DESCRIPTION: **inspectable**. It is correctly spelled, and it appears in the
same position in the Description field of version 0.3.0, which is on CRAN.

## Reverse dependencies

CRAN lists one reverse dependency: **bioLeak** (Suggests only; no reverse
depends, imports, or linking-to). CRAN's own pre-test reported "No strong
reverse dependencies to be checked."

This release contains breaking changes (removal of aliases deprecated in 0.2.0,
and of two constructors that were exported but never produced or consumed by the
package). bioLeak was checked against this version specifically:

* bioLeak uses none of the removed API. Its only integration point is
  `R/splitgraph_adapter.R`, which reads `spec$sample_data`, `spec$group_var`,
  `spec$constraint_mode` and `spec$time_var` — all unchanged in 0.4.0.
* No bioLeak test loads splitGraph, so its suite is unaffected.
* splitGraph ships a contract test (`tests/testthat/test-bioleak-contract.R`)
  that pins this boundary and passes against the installed bioLeak 0.3.8.

## Notes for the maintainers

* `Suggests: SummarizedExperiment` is a Bioconductor package. It is used by a
  single S3 method, which guards it with `requireNamespace()`. No example uses
  it; the one test that does calls `skip_if_not_installed()`, and the one
  vignette chunk is conditional on `requireNamespace()`. The package therefore
  checks cleanly without Bioconductor present.
* The package ships a small pure-Python reference reader under `inst/python/`.
  It is data, not compiled or executed at build or check time; the one test that
  runs it is skipped on CRAN.

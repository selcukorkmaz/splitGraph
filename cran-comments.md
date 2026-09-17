# cran-comments.md — splitGraph 0.4.0

This is an update of an existing CRAN package (0.3.0, published 2026-07-03).

## Test environments

<!--
  BEFORE SUBMITTING: run the remaining checks and replace the TODO lines with
  their results. Only the first line below has actually been run.
-->

* Local: Windows 11 x64, R 4.5.1 (2025-06-13 ucrt) — `R CMD check --as-cran`
  on the built tarball: 1 WARNING, 3 NOTEs, all local to that machine (see
  below).
* TODO win-builder, R-devel (`devtools::check_win_devel()`)
* TODO win-builder, R-release (`devtools::check_win_release()`)
* TODO macOS builder (`devtools::check_mac_release()`) or R-hub

## R CMD check results

The local run reports 0 errors | 1 warning | 3 notes. All four are caused by
tooling absent from the checking machine rather than by the package, and none is
expected on CRAN's own systems:

* WARNING `'qpdf' is needed for checks on size reduction of PDFs` — qpdf is not
  installed locally.
* NOTE `unable to verify current time` — the checking machine could not reach a
  time server.
* NOTE `Skipping checking HTML validation: no command 'tidy' found` — HTML Tidy
  is not installed locally.
* NOTE `detritus in the temp directory: 'lastMiKTeXException'` — a file left by
  MiKTeX, unrelated to this package.

`checking CRAN incoming feasibility` passes, and `urlchecker::url_check()`
reports all nine URLs in the package as valid. No spell-checker was available on
the machine used for these runs, so the `aspell` check on DESCRIPTION has not
been exercised locally.

## Reverse dependencies

CRAN lists one reverse dependency: **bioLeak** (Suggests only; no reverse
depends, imports, or linking-to).

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

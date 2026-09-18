# splitGraph release checklist

Run through this list, in order, for every CRAN release. Items marked *(CI)*
are also enforced by GitHub Actions; run them locally anyway before tagging.

## 1. Code and contract

- [ ] `NEWS.md` has a dated section for the release; every user-visible change
      in `git diff <last-tag>..HEAD` is mentioned. Breaking changes carry a
      migration table.
- [ ] `DESCRIPTION` `Version:` is the release version (no `.9000`).
- [ ] `.depgraph_schema_version` is correct for the on-disk contract:
      - unchanged if no `split_spec` / `dependency_graph` field was added,
      - bumped in the MINOR position for additive changes (same MAJOR: old
        files still load silently),
      - bumped in MAJOR only if an old file would become unreadable — and then
        `migrate_*_json()` and the Python reader's `_SUPPORTED_MAJORS` must
        follow.
- [ ] If the schema version changed: `inst/schema/<version>/` exists with both
      schema files, their `$id` points at the versioned path, the pinned strings
      in `tests/testthat/test-schema-version.R` and `test-json-schema.R` are
      updated, and a `<old> -> <new>` migration test exists.
- [ ] `inst/python/pyproject.toml` `version` tracks the schema version; the
      reader handles every field the schema declares.
- [ ] `?as_split_spec` "What downstream consumers read" and the README seam
      section still describe the current bioLeak release (check the tarball's
      `R/splitgraph_adapter.R`); `test-bioleak-contract.R` matches it.

## 2. Local checks

- [ ] `Rscript -e 'roxygen2::roxygenise()'` leaves no diff.
- [ ] `NOT_CRAN=true Rscript -e 'testthat::test_local()'` — 0 failures; the
      Python conformance test and the performance budget both *ran* (not
      skipped). *(CI)*
- [ ] `Rscript dev/coverage.R` ≥ 90 %. *(CI: test-coverage)*
- [ ] `Rscript inst/bench/pipeline.R 500 2000 5000 20000` — every step scales
      linearly; note the numbers in NEWS if they changed materially.
- [ ] `R CMD build .` then `R CMD check --as-cran splitGraph_<version>.tar.gz`.
      Install every Suggests first (bioLeak, SummarizedExperiment, rsample,
      jsonlite, knitr, rmarkdown) so nothing is skipped; do not reach for
      `_R_CHECK_FORCE_SUGGESTS_=false`, which hides those tests.
      *(CI: R-CMD-check, 5 platforms)*
- [ ] Known `--as-cran` output on a developer machine, and what each means:
      - `'qpdf' is needed for checks on size reduction of PDFs` — WARNING,
        environmental. Install qpdf, or accept it: CRAN's machines have it.
      - `Version contains large components` — NOTE, only while the version is
        `*.9000`. It disappears once bumped to the release version.
      - `Files 'README.md' or 'NEWS.md' cannot be checked without 'pandoc'` —
        NOTE. Put pandoc on PATH (`install.packages("pandoc");
        pandoc::pandoc_install()` gives one, but export its directory on PATH
        for the check subprocess, not just `RSTUDIO_PANDOC`).
      - DESCRIPTION's `URL` deliberately lists **only** the GitHub repository.
        The pkgdown site URL was removed for the 0.4.0 submission because it
        404s until the site is deployed, and CRAN's incoming check rejects
        that. Once the `pkgdown` workflow has run and Pages is enabled, add
        `https://selcukorkmaz.github.io/splitGraph/` back to `URL` and
        re-roxygenise. `_pkgdown.yml` keeps its own `url:` either way, so the
        site builds correctly in the meantime.
- [ ] `Rscript -e 'lintr::lint_package()'` reports 0 findings (the committed
      `.lintr` is calibrated so it is a real gate, not noise).
- [ ] pkgdown site builds: `pkgdown::build_site(install = FALSE)` with the
      package installed in a library on the path. Output goes to
      `pkgdown-site/` (git-ignored), not `docs/`, which holds the design
      blueprint.
- [ ] Vignettes build (pandoc required) or, without pandoc, every vignette's
      code executes: `Rscript -e 'for (v in list.files("vignettes", "[.]Rmd$", full.names=TRUE)) { f <- tempfile(fileext=".R"); knitr::purl(v, f, quiet=TRUE, documentation=0); source(f, echo=FALSE) }'`.
- [ ] `urlchecker::url_check()` clean.
- [ ] `lintr::lint_package()` reports nothing new.

## 3. Line endings and the tarball

- [ ] Build the release tarball on Linux (CI) or, on Windows, verify no CRLF
      leaked in: `tar -xzf splitGraph_<version>.tar.gz && grep -rlI $'\r' splitGraph/ | head` must print nothing.
      `.gitattributes` normalises only the git index, not the working tree.
- [ ] The tarball contains no `MD5` (CRAN adds it), no `docs/`, `dev/`,
      `ROADMAP-*.md`, `paper.*`, `_pkgdown.yml`, `.lintr`, `inst/python/__pycache__`.

## 4. Reverse dependencies

- [ ] `revdepcheck::revdep_check()`, or manually: install this splitGraph into a
      temporary library, then run bioLeak's own test suite against it. Last run
      2026-09-16 against bioLeak 0.3.8: 281 tests, 0 failures, 3 skipped.
      Also run splitGraph's own suite with `NOT_CRAN=true` and bioLeak present;
      the contract tests must pass and must not skip.
- [ ] Multi-platform: `rhub::rhub_check()` on at least Windows, macOS, and
      Linux devel.

## 5. Ship

- [ ] Push the branch and let the `pkgdown` workflow finish. This no longer
      blocks submission -- the site URL is out of DESCRIPTION as of 0.4.0 -- but
      the site should exist before the release is announced. After Pages is
      live, put the URL back in DESCRIPTION, re-roxygenise, and confirm with
      `urlchecker::url_check(".")` that it resolves.
- [ ] `inst/CITATION` and the README citation block name the new version.
      (CITATION reads `meta[["Version"]]`, so it follows DESCRIPTION; the README
      block is hand-written and must be edited.)
- [ ] `git tag v<version>`; push tag after CRAN acceptance.
- [ ] Bump `DESCRIPTION` to `<version>.9000` and open a
      `# splitGraph (development version)` section in NEWS.
- [ ] If the bioLeak seam changed (Workstream D2), coordinate the bioLeak
      release and then flip the pinned contract test.

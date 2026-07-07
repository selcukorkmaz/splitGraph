# Contributing to splitGraph

Thanks for your interest in improving **splitGraph**. This document explains how
to contribute code, how to report problems, and how to get help.

## Seeking support

If you have a question about how to use the package:

- Read the vignettes: `browseVignettes("splitGraph")` (or the package
  [website](https://github.com/selcukorkmaz/splitGraph)). The
  *leakage-aware-workflow* vignette is the best starting point.
- Search the [issue tracker](https://github.com/selcukorkmaz/splitGraph/issues)
  in case your question has already been answered.
- If you still need help, open a
  [new issue](https://github.com/selcukorkmaz/splitGraph/issues/new) with the
  `question` label. Usage questions are welcome there.

## Reporting issues and bugs

Please file bugs and feature requests on the
[issue tracker](https://github.com/selcukorkmaz/splitGraph/issues).

A good bug report includes:

1. A short description of what you expected and what happened instead.
2. A **minimal reproducible example** — the smallest metadata table and sequence
   of `splitGraph` calls that triggers the problem. The
   [reprex](https://reprex.tidyverse.org/) package is a convenient way to
   produce one.
3. The output of `sessionInfo()` (or at least your R version and the installed
   `splitGraph` version, `packageVersion("splitGraph")`).

For anything that looks like a security or data-integrity concern (for example a
split that silently leaks structure), please say so explicitly in the report.

## Contributing code

We welcome pull requests. For anything larger than a small fix, please open an
issue first so we can agree on the approach before you invest time.

### Development setup

```r
install.packages(c("devtools", "roxygen2", "testthat"))
# from a clone of the repository:
devtools::load_all()      # load the package for interactive work
devtools::test()          # run the test suite
devtools::document()      # regenerate NAMESPACE and man/*.Rd from roxygen
devtools::check()         # run R CMD check
```

### Pull request guidelines

- Branch from `main` and keep each pull request focused on one change.
- **Add tests.** New behaviour needs a `testthat` test; bug fixes should include
  a regression test that fails before the fix. Please keep coverage from
  regressing.
- **Document exported functions** with roxygen and run `devtools::document()` so
  `NAMESPACE` and the `man/` pages stay in sync.
- Update `NEWS.md` with a user-facing summary of your change.
- Match the surrounding code style (base-R idioms, no new hard dependencies —
  the package intentionally has a minimal `Imports` footprint).
- Respect the package's scope: splitGraph represents dependency structure,
  validates it, derives split **constraints**, and emits the `split_spec`
  interchange format. It deliberately does **not** generate resamples, fit
  models, apply purge/embargo, or compute statistical leakage evidence — those
  belong to downstream consumers. Changes that cross that boundary are unlikely
  to be merged.
- Make sure `devtools::check()` passes with no new errors, warnings, or notes.

By contributing, you agree that your contributions are licensed under the
project's MIT License, and you are expected to uphold the
[Code of Conduct](../CODE_OF_CONDUCT.md).

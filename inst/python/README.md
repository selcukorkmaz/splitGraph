# splitspec -- Python reference consumer for splitGraph `split_spec`

A pure-Python reader for the tool-agnostic `split_spec` JSON interchange format
written by `splitGraph::write_split_spec()`. It recovers the sample-level
grouping, ordering, and stratum annotations needed to drive scikit-learn
resamplers, and is the reference implementation used by the cross-language
conformance check.

- The reader (`splitspec.load_split_spec`) and `SplitSpec.iter_splits()` depend only on the standard library.
- `SplitSpec.to_frame()` additionally needs `pandas`.
- `SplitSpec.group_kfold()` and `SplitSpec.stratified_group_kfold()` import scikit-learn (and numpy) lazily.

## Usage

```python
import sys
sys.path.insert(0, "<path to this inst/python directory>")
from splitspec import load_split_spec

spec = load_split_spec("split_spec.json")
spec.grouping()      # {sample_id: group_id}, == R grouping_vector()
spec.groups()        # group_id per sample, for GroupKFold(groups=...)
spec.order_ranks()   # order_rank per sample, for TimeSeriesSplit
spec.strata()        # stratum per sample (spec.stratum_var), for StratifiedGroupKFold(y=...)
spec.to_frame()      # pandas DataFrame of sample_data

for train_idx, test_idx in spec.iter_splits(n_splits=5):
    ...               # stdlib grouped/stratified k-fold or forward-chaining

for train_idx, test_idx in spec.stratified_group_kfold(n_splits=5):
    ...               # grouped on group_id, stratified on the stratum annotation
```

When `spec.ordering_required` is `True` (or `spec.recommended_resampling == "forward"`),
`spec.iter_splits(n_splits=..., purge_ranks=...)` constructs forward-chaining
splits over `order_rank` (with an integral `n_splits >= 2` and a finite,
nonnegative `purge_ranks` gap in `order_rank` units). Every emitted split is
verified to have non-empty training and test sets, disjoint row indices and
declared groups, and `max(train rank) < min(test rank) - purge_ranks`. In
longitudinal panels where a recurring group spans both the initial training
window and the test window, the training side is filtered first and, if groups
still overlap, the test window is narrowed as a fallback (which can reduce test
coverage across windows). If any requested split cannot satisfy these
invariants, `iter_splits()` raises a `ValueError` before emitting any split.

`stratum_var` / `stratum` were added in schema 0.3.0 (splitGraph 0.4.0); files
written by earlier versions load with `spec.stratum_var is None` and
`spec.strata()` returning `None` for every sample. The reader requires a valid
`"X.Y.Z"` `schema_version` string and accepts every schema with major version `0`.

## Installing as a package

`pyproject.toml` in this directory packages the reader as `splitspec`:

```bash
pip install <path to this inst/python directory>          # reader only
pip install "<path to this inst/python directory>[sklearn]" # + scikit-learn helpers
```

In R, locate this directory with:

```r
system.file("python", package = "splitGraph")
```

## Conformance

`conformance.py <split_spec.json> <out.json>` reads a spec and writes back the
grouping and ordering it recovered. The R test `test-python-conformance.R`
runs it and asserts the result matches `grouping_vector()` and
`sample_data$order_rank` exactly. It is skipped when `python3` is unavailable
and never runs on CRAN.

## Compatibility

The reader accepts any `split_spec` sharing schema **major** version `0`
(the compatibility boundary), matching the R side's policy.

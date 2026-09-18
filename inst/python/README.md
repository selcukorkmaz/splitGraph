# splitspec — Python reference consumer for splitGraph `split_spec`

A pure-Python reader for the tool-agnostic `split_spec` JSON interchange format
written by `splitGraph::write_split_spec()`. It recovers the sample-level
grouping, ordering, and stratum annotations needed to drive scikit-learn
resamplers, and is the reference implementation used by the cross-language
conformance check.

- The reader (`splitspec.load_split_spec`) depends only on the standard library.
- `SplitSpec.to_frame()` additionally needs `pandas`.
- `SplitSpec.group_kfold()` imports scikit-learn (and numpy) lazily.

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

for train_idx, test_idx in spec.stratified_group_kfold(n_splits=5):
    ...               # grouped on group_id, stratified on the stratum annotation
```

`stratum_var` / `stratum` were added in schema 0.3.0 (splitGraph 0.4.0); files
written by earlier versions load with `spec.stratum_var is None` and
`spec.strata()` returning `None` for every sample. The reader accepts every
schema with major version 0.

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

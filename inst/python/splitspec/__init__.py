"""splitspec: a pure-Python reference consumer for splitGraph ``split_spec`` JSON.

This package reads the tool-agnostic ``split_spec`` interchange format written
by ``splitGraph::write_split_spec()`` and exposes the sample-level grouping,
ordering, and stratum annotations needed to drive scikit-learn resamplers
(``GroupKFold``, ``StratifiedGroupKFold``, ``TimeSeriesSplit``). The reader
itself depends only on the Python standard library; ``to_frame()`` additionally
needs ``pandas``, and the scikit-learn helpers import scikit-learn lazily.
"""

from .reader import SplitSpec, load_split_spec

__all__ = ["SplitSpec", "load_split_spec"]
__version__ = "0.2.0"

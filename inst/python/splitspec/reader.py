"""Reader for the splitGraph ``split_spec`` JSON interchange format."""

import json

# Schema major versions this reader understands. The major component is the
# compatibility boundary, mirroring the R side's policy.
_SUPPORTED_MAJORS = {0}


def _major(version):
    if not version:
        return None
    try:
        return int(str(version).split(".")[0])
    except (ValueError, IndexError):
        return None


class SplitSpec:
    """A parsed ``split_spec``.

    Attributes mirror the top-level JSON fields. ``sample_data`` is the list of
    per-sample row dicts, preserved in file order.
    """

    def __init__(self, data):
        obj = data.get("splitGraph_object")
        if obj != "split_spec":
            raise ValueError(
                "Not a split_spec document (splitGraph_object=%r)." % obj
            )

        self.schema_version = data.get("schema_version")
        major = _major(self.schema_version)
        if major is not None and major not in _SUPPORTED_MAJORS:
            raise ValueError(
                "Unsupported split_spec schema major version %r (this reader "
                "supports majors %s)." % (self.schema_version, sorted(_SUPPORTED_MAJORS))
            )

        self.group_var = data.get("group_var", "group_id")
        self.block_vars = list(data.get("block_vars") or [])
        self.time_var = data.get("time_var")
        self.ordering_required = bool(data.get("ordering_required"))
        self.constraint_mode = data.get("constraint_mode")
        self.constraint_strategy = data.get("constraint_strategy")
        self.recommended_resampling = data.get("recommended_resampling")
        self.metadata = data.get("metadata") or {}
        self.sample_data = list(data.get("sample_data") or [])

    # ---- basic accessors (stdlib only) --------------------------------------

    @property
    def sample_ids(self):
        return [row.get("sample_id") for row in self.sample_data]

    def groups(self):
        """Grouping vector (``group_var`` per sample), in file order.

        Pass directly as the ``groups=`` argument of ``GroupKFold.split()`` or
        ``StratifiedGroupKFold.split()``.
        """
        return [row.get(self.group_var) for row in self.sample_data]

    def order_ranks(self):
        """Ordering vector (``order_rank`` per sample), in file order.

        Sort the frame by this before applying ``TimeSeriesSplit``. Entries may
        be ``None`` when no ordering was derived.
        """
        return [row.get("order_rank") for row in self.sample_data]

    def strata(self, column):
        """Stratum annotation from ``column`` (e.g. a block variable)."""
        return [row.get(column) for row in self.sample_data]

    def grouping(self):
        """Mapping ``sample_id -> group_id``.

        Equivalent to R's ``grouping_vector(constraint)``; the primary handle
        for conformance checks between the two languages.
        """
        return {row.get("sample_id"): row.get(self.group_var) for row in self.sample_data}

    def ordered_index(self):
        """Row indices sorted by ``order_rank`` (stable), missing ranks last.

        Use to reorder the frame before ``TimeSeriesSplit``.
        """
        ranks = self.order_ranks()
        with_rank = [i for i, r in enumerate(ranks) if r is not None]
        without_rank = [i for i, r in enumerate(ranks) if r is None]
        with_rank.sort(key=lambda i: ranks[i])
        return with_rank + without_rank

    # ---- pandas / scikit-learn convenience (optional deps) ------------------

    def to_frame(self):
        """Return ``sample_data`` as a :class:`pandas.DataFrame` (needs pandas)."""
        import pandas as pd

        return pd.DataFrame(self.sample_data)

    def group_kfold(self, n_splits=5):
        """Yield ``(train_idx, test_idx)`` from ``sklearn.GroupKFold``.

        Keyed on the grouping vector; needs scikit-learn and numpy.
        """
        import numpy as np
        from sklearn.model_selection import GroupKFold

        n = len(self.sample_data)
        X = np.zeros((n, 1))
        return GroupKFold(n_splits=n_splits).split(X, groups=self.groups())


def load_split_spec(path):
    """Read a ``split_spec`` JSON file and return a :class:`SplitSpec`."""
    with open(path, "r", encoding="utf-8") as handle:
        data = json.load(handle)
    return SplitSpec(data)

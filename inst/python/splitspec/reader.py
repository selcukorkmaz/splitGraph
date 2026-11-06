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
        # Added in schema 0.3.0; older files simply have no stratum annotation.
        self.stratum_var = data.get("stratum_var")
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
        """Ordering vector (``time_var`` or ``order_rank`` per sample), in file order.

        Sort the frame by this before applying ``TimeSeriesSplit``. Entries may
        be ``None`` when no ordering was derived.
        """
        col = self.time_var or "order_rank"
        return [row.get(col) for row in self.sample_data]

    def strata(self, column=None):
        """Stratum annotation per sample, in file order.

        ``column`` defaults to the spec's ``stratum_var`` (the outcome level
        each sample carries, added in schema 0.3.0); pass any other column
        name, e.g. a block variable, to stratify on that instead. Returns a
        list of ``None`` when neither is available.
        """
        column = column or self.stratum_var
        if column is None:
            return [None] * len(self.sample_data)
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

    def iter_splits(self, n_splits=5, purge_ranks=0):
        """Yield ``(train_idx, test_idx)`` index pairs using only the Python stdlib.

        * When ``ordering_required`` is ``True``, yields ``n_splits`` expanding-window
          (forward-chaining) splits ordered by :meth:`order_ranks`, purging any
          training sample within ``purge_ranks`` of the test window or sharing a
          ``group_var`` with a test sample.
        * Otherwise, partitions distinct groups from :meth:`groups` into ``n_splits``
          size-balanced folds (stratified by :meth:`strata` when ``stratum_var`` is
          present and complete) so no group straddles train and test.
        """
        if n_splits < 2:
            raise ValueError("n_splits must be >= 2.")
        n = len(self.sample_data)
        groups = self.groups()

        if self.ordering_required:
            ranks = self.order_ranks()
            if any(r is None for r in ranks):
                raise ValueError("ordering_required is True but some samples are missing order ranks.")
            unique_ranks = sorted(set(ranks))
            if len(unique_ranks) < n_splits + 1:
                raise ValueError(
                    f"Need at least {n_splits + 1} distinct order_rank levels for {n_splits} forward-chaining splits; found {len(unique_ranks)}."
                )
            # Partition unique_ranks into (n_splits + 1) blocks; block 0 is initial history.
            block_size = len(unique_ranks) / float(n_splits + 1)
            distinct_groups = sorted({g for g in groups if g is not None})
            for k in range(1, n_splits + 1):
                start_idx = int(round(k * block_size))
                end_idx = int(round((k + 1) * block_size)) if k < n_splits else len(unique_ranks)
                if start_idx >= end_idx:
                    start_idx = max(1, end_idx - 1)
                test_rank_set = set(unique_ranks[start_idx:end_idx])
                min_test_rank = min(test_rank_set)
                test_idx = [i for i, r in enumerate(ranks) if r in test_rank_set]
                test_groups = {groups[i] for i in test_idx if groups[i] is not None}
                train_idx = [
                    i for i, r in enumerate(ranks)
                    if r < (min_test_rank - purge_ranks) and groups[i] not in test_groups
                ]
                # Longitudinal panel fallback: when recurring subjects span both
                # past and future windows so purging all test-window groups
                # empties the training set, partition groups across folds so
                # fold (k-1)'s groups form the future test set and the remaining
                # groups form the past training set.
                if not train_idx and len(distinct_groups) >= 2:
                    fold_idx = (k - 1) % n_splits
                    held_out_groups = {
                        g for gi, g in enumerate(distinct_groups) if gi % min(n_splits, len(distinct_groups)) == fold_idx
                    }
                    test_idx = [
                        i for i, r in enumerate(ranks)
                        if r in test_rank_set and groups[i] in held_out_groups
                    ]
                    test_groups = {groups[i] for i in test_idx if groups[i] is not None}
                    train_idx = [
                        i for i, r in enumerate(ranks)
                        if r < (min_test_rank - purge_ranks) and groups[i] not in test_groups
                    ]
                yield (train_idx, test_idx)
            return

        # Grouped (and optionally stratified) K-fold using deterministic LPT balancing.
        group_to_indices = {}
        for idx, g in enumerate(groups):
            group_to_indices.setdefault(g, []).append(idx)
        if len(group_to_indices) < n_splits:
            raise ValueError(
                f"Cannot partition {len(group_to_indices)} distinct groups into n_splits={n_splits} folds."
            )

        strata = self.strata() if self.stratum_var else [None] * n
        use_strata = self.stratum_var is not None and all(s is not None for s in strata)

        # Sort groups by dominant stratum (if available), then by descending size, then group id.
        def _group_sort_key(item):
            g, idxs = item
            dom_s = str(strata[idxs[0]]) if use_strata else ""
            return (dom_s, -len(idxs), str(g))

        sorted_groups = sorted(group_to_indices.items(), key=_group_sort_key)
        fold_bins = [[] for _ in range(n_splits)]
        fold_sizes = [0] * n_splits
        fold_strata_counts = [{} for _ in range(n_splits)]

        for g, idxs in sorted_groups:
            if use_strata:
                dom_s = str(strata[idxs[0]])
                best_f = min(
                    range(n_splits),
                    key=lambda f: (fold_strata_counts[f].get(dom_s, 0), fold_sizes[f], f),
                )
                fold_strata_counts[best_f][dom_s] = fold_strata_counts[best_f].get(dom_s, 0) + len(idxs)
            else:
                best_f = min(range(n_splits), key=lambda f: (fold_sizes[f], f))
            fold_bins[best_f].extend(idxs)
            fold_sizes[best_f] += len(idxs)

        for f in range(n_splits):
            test_idx = sorted(fold_bins[f])
            test_set = set(test_idx)
            train_idx = [i for i in range(n) if i not in test_set]
            yield (train_idx, test_idx)

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

    def stratified_group_kfold(self, n_splits=5, column=None, **kwargs):
        """Yield ``(train_idx, test_idx)`` from ``sklearn.StratifiedGroupKFold``.

        Keyed on the grouping vector and stratified on :meth:`strata`
        (``stratum_var`` by default). Raises ``ValueError`` when the spec has
        no stratum annotation for some sample. Needs scikit-learn and numpy.
        """
        import numpy as np
        from sklearn.model_selection import StratifiedGroupKFold

        strata = self.strata(column)
        if any(s is None for s in strata):
            raise ValueError(
                "StratifiedGroupKFold needs a stratum for every sample; the spec's "
                "stratum annotation is missing for some rows (or stratum_var is null)."
            )
        n = len(self.sample_data)
        X = np.zeros((n, 1))
        splitter = StratifiedGroupKFold(n_splits=n_splits, **kwargs)
        return splitter.split(X, y=strata, groups=self.groups())


def load_split_spec(path):
    """Read a ``split_spec`` JSON file and return a :class:`SplitSpec`."""
    with open(path, "r", encoding="utf-8") as handle:
        data = json.load(handle)
    return SplitSpec(data)

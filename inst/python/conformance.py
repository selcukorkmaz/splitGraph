"""Cross-language conformance helper.

Reads a ``split_spec`` JSON written by R and emits, as JSON, the grouping and
ordering the Python reader recovers from it. The R conformance test compares
this against ``grouping_vector()`` and ``sample_data$order_rank`` to prove the
two implementations agree on the interchange format.

Usage:
    python3 conformance.py <split_spec.json> <out.json>
"""

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from splitspec import load_split_spec  # noqa: E402


def main(in_path, out_path):
    spec = load_split_spec(in_path)
    result = {
        "schema_version": spec.schema_version,
        "group_var": spec.group_var,
        "n_samples": len(spec.sample_data),
        "grouping": spec.grouping(),
        "order_ranks": dict(zip(spec.sample_ids, spec.order_ranks())),
    }
    with open(out_path, "w", encoding="utf-8") as handle:
        json.dump(result, handle)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.stderr.write("usage: conformance.py <split_spec.json> <out.json>\n")
        sys.exit(2)
    main(sys.argv[1], sys.argv[2])

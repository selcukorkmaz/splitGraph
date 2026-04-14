\name{as_split_spec}
\alias{as_split_spec}
\alias{validate_split_spec}
\alias{summarize_leakage_risks}
\alias{split_spec}
\alias{split_spec_validation}
\alias{leakage_risk_summary}
\title{Translate splitGraph Constraints into Stable Split Specifications}
\usage{
as_split_spec(constraint, graph = NULL)

validate_split_spec(x)

summarize_leakage_risks(graph, constraint = NULL, split_spec = NULL,
  validation = NULL)
}
\arguments{
  \item{constraint}{A \code{split_constraint}.}
  \item{graph}{A \code{dependency_graph}.}
  \item{split_spec}{An optional \code{split_spec}.}
  \item{validation}{An optional \code{depgraph_validation_report}.}
  \item{x}{A \code{split_spec}.}
}
\value{
\code{as_split_spec()} returns a \code{split_spec}.
\code{validate_split_spec()} returns a \code{split_spec_validation}.
\code{summarize_leakage_risks()} returns a \code{leakage_risk_summary}.
}
\description{
Translate graph-derived split constraints into a stable, inspectable structure
for sample-level grouping, blocking, and ordering, perform preflight
structural checks on that translation, and summarize structural leakage risks.
}
\details{
The translation layer always produces canonical sample-level columns including
\code{sample_id}, \code{sample_node_id}, \code{group_id}, and
\code{primary_group}. When available, it also carries \code{batch_group},
\code{study_group}, \code{timepoint_id}, \code{time_index}, and
\code{order_rank}. Missing but relevant fields are retained as \code{NA}
columns rather than omitted.

When only a subset of samples has ordering metadata, the translated split spec
still exposes that partial ordering through \code{time_var}, but
\code{ordering_required} remains \code{FALSE}. Ordering is only marked as
required when the constraint implies complete ordering coverage.

The split-spec validator checks:
\itemize{
  \item missing required columns
  \item missing or duplicated sample identifiers
  \item missing grouping assignments
  \item singleton-only grouping structures
  \item missing ordering when ordering is required
  \item invalid or empty block variables
}

Repeated validation of the same split spec yields deterministic issue IDs and
diagnostics, which makes the returned validation object stable across runs.

The produced \code{split_spec} is tool-agnostic. Downstream consumers
(\pkg{bioLeak}, \pkg{fastml}, \pkg{rsample}, ...) provide their own adapters
to convert a \code{split_spec} into their native split representation, so
\pkg{splitGraph} has no runtime dependency on any of them.

\code{summarize_leakage_risks()} reuses \code{validate_graph()} and
\code{split_constraint} metadata rather than duplicating downstream
evaluation logic.
}

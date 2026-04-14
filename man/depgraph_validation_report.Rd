\name{depgraph_validation_report}
\alias{depgraph_validation_report}
\title{Validation Report Object for splitGraph Graphs}
\description{
\code{depgraph_validation_report} is the structured return type produced by
\code{validate_graph()} and \code{validate_depgraph()}.
}
\details{
The report contains:

\itemize{
  \item \code{graph_name}: graph label when available
  \item \code{valid}: whether any \code{error}-severity issues were found
  \item \code{issues}: canonical issue table
  \item \code{summary}: counts by level, severity, and code
  \item \code{metadata}: report metadata
  \item \code{errors}, \code{warnings}, \code{advisories}: backward-compatible message vectors
  \item \code{metrics}: graph and issue counts
}

The canonical issue table includes the columns:
\code{issue_id}, \code{level}, \code{severity}, \code{code}, \code{message},
\code{node_ids}, \code{edge_ids}, and \code{details}.
}
\seealso{
\code{\link{validate_graph}}
}
\examples{
meta <- data.frame(
  sample_id  = c("S1", "S2"),
  subject_id = c("P1", "P2")
)
g <- graph_from_metadata(meta)

report <- validate_graph(g)
report$valid
summary(report)
}

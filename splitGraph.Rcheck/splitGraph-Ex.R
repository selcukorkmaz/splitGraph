pkgname <- "splitGraph"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('splitGraph')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_dependency_graph")
### * build_dependency_graph

flush(stderr()); flush(stdout())

### Name: build_dependency_graph
### Title: Assemble and Validate Dependency Graphs
### Aliases: build_dependency_graph build_depgraph validate_graph
###   validate_depgraph as_igraph

### ** Examples

meta <- data.frame(
  sample_id = c("S1", "S2"),
  subject_id = c("P1", "P2")
)

samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
edges <- create_edges(
  meta,
  "sample_id",
  "subject_id",
  "Sample",
  "Subject",
  "sample_belongs_to_subject"
)

g <- build_dependency_graph(list(samples, subjects), list(edges))
validate_graph(g)



cleanEx()
nameEx("create_nodes")
### * create_nodes

flush(stderr()); flush(stdout())

### Name: create_nodes
### Title: Create Canonical Node and Edge Tables
### Aliases: create_nodes create_edges

### ** Examples

meta <- data.frame(
  sample_id = c("S1", "S2"),
  subject_id = c("P1", "P2")
)

samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
edges <- create_edges(
  meta,
  from_col = "sample_id",
  to_col = "subject_id",
  from_type = "Sample",
  to_type = "Subject",
  relation = "sample_belongs_to_subject"
)



cleanEx()
nameEx("ingest_metadata")
### * ingest_metadata

flush(stderr()); flush(stdout())

### Name: ingest_metadata
### Title: Standardize Sample Metadata
### Aliases: ingest_metadata

### ** Examples

meta <- ingest_metadata(
  data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"))
)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

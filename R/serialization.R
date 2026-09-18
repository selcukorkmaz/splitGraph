# JSON serialization for splitGraph's two handoff objects:
# `dependency_graph` and `split_spec`. The on-disk format is documented in
# the @section JSON format blocks below and is intended to be stable across
# patch releases. Schema bumps follow the rules described in
# `R/splitGraph-package.R` (`.depgraph_schema_version`).

.depgraph_require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    .depgraph_stop(c(
      "Package 'jsonlite' is required for splitGraph JSON serialization. ",
      "Install it with: install.packages(\"jsonlite\")."
    ))
  }
  invisible(TRUE)
}

.depgraph_check_writable_path <- function(path) {
  parent <- dirname(path)
  if (!nzchar(parent) || identical(parent, ".")) return(invisible())
  if (!dir.exists(parent)) {
    .depgraph_stop(
      c("Parent directory does not exist: ", parent, ". Create it before writing."),
      class = "splitgraph_io_error", code = "missing_parent_directory"
    )
  }
  invisible()
}

# Convert a POSIXct (or NULL/NA) to an ISO 8601 string; round-trip via
# .depgraph_iso_to_posix.
.depgraph_posix_to_iso <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return(NA_character_)
  }
  format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS6%z")
}

.depgraph_iso_to_posix <- function(x) {
  na_posix <- .POSIXct(NA_real_, tz = "UTC")
  if (is.null(x) || length(x) == 0L) {
    return(na_posix)
  }
  if (length(x) == 1L && is.na(x)) {
    return(na_posix)
  }
  try_parse <- function(call_expr) {
    tryCatch(
      suppressWarnings(call_expr),
      error = function(e) na_posix
    )
  }
  parsed <- try_parse(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS%z"))
  if (length(parsed) == 0L || is.na(parsed)) {
    parsed <- try_parse(as.POSIXct(x))
  }
  if (length(parsed) == 0L || is.na(parsed)) na_posix else parsed
}

# Public `$id` of the shipped JSON Schema for an object type, referenced from
# written JSON via the `$schema` key so external consumers can locate the
# formal contract. Mirrors the file names under `inst/schema/`.
.depgraph_schema_url <- function(object_type, version = .depgraph_schema_version) {
  paste0(
    "https://raw.githubusercontent.com/selcukorkmaz/splitGraph/main/inst/schema/",
    version, "/", object_type, ".schema.json"
  )
}

# Major-version component of a "X.Y.Z" schema string; NA if unparseable.
.depgraph_schema_major <- function(version) {
  major <- suppressWarnings(as.integer(sub("\\..*$", "", as.character(version)[1L])))
  major
}

.depgraph_check_schema_version <- function(observed, what) {
  if (is.null(observed) || !nzchar(observed)) {
    .depgraph_warn(
      c("Reading ", what, ": no `schema_version` recorded in JSON. ",
        "Assuming current schema (", .depgraph_schema_version, ")."),
      code = "missing_schema_version"
    )
    return(invisible())
  }

  observed <- as.character(observed)
  if (identical(observed, .depgraph_schema_version)) {
    return(invisible())
  }

  observed_major <- .depgraph_schema_major(observed)
  installed_major <- .depgraph_schema_major(.depgraph_schema_version)

  # Same MAJOR is read-compatible: additive-only differences, load silently.
  if (!is.na(observed_major) && identical(observed_major, installed_major)) {
    return(invisible())
  }

  migrator <- if (identical(what, "split_spec")) {
    "migrate_split_spec_json()"
  } else {
    "migrate_dependency_graph_json()"
  }
  .depgraph_warn(
    c("Reading ", what, ": JSON schema_version `", observed,
      "` differs in major version from installed splitGraph schema_version `",
      .depgraph_schema_version, "`. Loading anyway; consider `", migrator,
      "` to upgrade the file."),
    code = "schema_major_mismatch"
  )
  invisible()
}

# Convert an attrs list (one row's named list) into a JSON-friendly list.
# Empty attrs become an empty named list (jsonlite emits `{}`); scalars stay
# as scalars.
.depgraph_attrs_to_json <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(structure(list(), names = character()))
  }
  as.list(x)
}

# Reverse: a parsed JSON object becomes a named list (empty -> list()).
.depgraph_attrs_from_json <- function(x) {
  if (is.null(x)) return(list())
  if (!is.list(x)) return(list())
  if (length(x) == 0L) return(list())
  x
}

# Table builders for the JSON writers. jsonlite serialises a data frame with
# `dataframe = "rows"` (its default) as an array of row objects; handing it a
# data frame is ~3x faster than building nested R lists, and the emitted text is
# identical. The `attrs` list-column carries one named list per row; an empty
# entry is a *named* empty list so it prints as `{}` (the schema types `attrs`
# as an object), never `[]`. An empty table returns `list()` so it prints `[]`.
.depgraph_node_rows_to_json <- function(node_data) {
  if (nrow(node_data) == 0L) return(list())
  out <- data.frame(
    node_id = node_data$node_id,
    node_type = node_data$node_type,
    node_key = node_data$node_key,
    label = node_data$label,
    stringsAsFactors = FALSE
  )
  out$attrs <- lapply(node_data$attrs, .depgraph_attrs_to_json)
  out
}

.depgraph_edge_rows_to_json <- function(edge_data) {
  if (nrow(edge_data) == 0L) return(list())
  out <- data.frame(
    edge_id = edge_data$edge_id,
    from = edge_data$from,
    to = edge_data$to,
    edge_type = edge_data$edge_type,
    stringsAsFactors = FALSE
  )
  out$attrs <- lapply(edge_data$attrs, .depgraph_attrs_to_json)
  out
}

# Serialize graph metadata, dropping fields that are not portable
# (validation_overrides IS preserved; igraph object is not stored).
.depgraph_metadata_to_json <- function(metadata) {
  out <- list(
    graph_name           = metadata$graph_name %||% NA_character_,
    dataset_name         = metadata$dataset_name %||% NA_character_,
    created_at           = .depgraph_posix_to_iso(metadata$created_at),
    schema_version       = metadata$schema_version %||% .depgraph_schema_version,
    # An *unnamed* empty list would serialise as `[]`; the schema declares an
    # object, so force the named empty list (`{}`) whenever there is nothing in it.
    validation_overrides = if (is.null(metadata$validation_overrides) || length(metadata$validation_overrides) == 0L) {
      structure(list(), names = character())
    } else {
      as.list(metadata$validation_overrides)
    },
    edge_sources         = if (is.null(metadata$edge_sources) || length(metadata$edge_sources) == 0L) {
      structure(list(), names = character())
    } else {
      lapply(metadata$edge_sources, as.list)
    }
  )
  out
}

.depgraph_metadata_from_json <- function(meta_list) {
  if (is.null(meta_list)) meta_list <- list()
  out <- list(
    graph_name     = if (is.null(meta_list$graph_name) || is.na(meta_list$graph_name)) NULL else as.character(meta_list$graph_name),
    dataset_name   = if (is.null(meta_list$dataset_name) || is.na(meta_list$dataset_name)) NULL else as.character(meta_list$dataset_name),
    created_at     = .depgraph_iso_to_posix(meta_list$created_at),
    schema_version = meta_list$schema_version %||% .depgraph_schema_version
  )
  if (!is.null(meta_list$validation_overrides) && length(meta_list$validation_overrides) > 0L) {
    out$validation_overrides <- as.list(meta_list$validation_overrides)
  }
  if (!is.null(meta_list$edge_sources) && length(meta_list$edge_sources) > 0L) {
    out$edge_sources <- lapply(meta_list$edge_sources, as.list)
  }
  out
}

# ---- Public API: dependency_graph -------------------------------------------

#' Serialize a Dependency Graph to JSON
#'
#' Write a \code{dependency_graph} to a JSON file and read it back. The on-disk
#' format is intentionally simple and stable: it captures the canonical node
#' table, the canonical edge table (each with their list-column of
#' attributes), the graph metadata (including \code{validation_overrides}),
#' and the data-model \code{schema_version}. The internal \code{igraph}
#' representation is not stored; it is rebuilt on read via
#' \code{dependency_graph()}.
#'
#' This makes \code{split_spec}/\code{dependency_graph} objects portable
#' across R sessions, and across language boundaries (any consumer that can
#' read JSON can interpret the format).
#'
#' @section JSON format:
#' \preformatted{
#' {
#'   "$schema": "https://.../inst/schema/0.3.0/dependency_graph.schema.json",
#'   "splitGraph_object": "dependency_graph",
#'   "schema_version": "0.3.0",
#'   "metadata": {
#'     "graph_name": "...",
#'     "dataset_name": "...",
#'     "created_at": "2026-04-29T10:11:12.000000+0000",
#'     "schema_version": "0.3.0",
#'     "validation_overrides": { ... },
#'     "edge_sources": {
#'       "subject_related_to": { "relation": "...", "from_col": "...",
#'                               "to_col": "...", "threshold": 0.125,
#'                               "metric": "kinship" }
#'     }
#'   },
#'   "nodes": [
#'     { "node_id": "sample:S1", "node_type": "Sample",
#'       "node_key": "S1", "label": "S1", "attrs": { ... } },
#'     ...
#'   ],
#'   "edges": [
#'     { "edge_id": "sample_belongs_to_subject:1",
#'       "from": "sample:S1", "to": "subject:P1",
#'       "edge_type": "sample_belongs_to_subject", "attrs": { ... } },
#'     ...
#'   ]
#' }
#' }
#' Reading a file whose \code{schema_version} shares the installed major
#' version loads silently (additive-only differences); a differing major
#' version loads with a warning suggesting \code{migrate_dependency_graph_json()}.
#' The written JSON also carries a \code{$schema} reference to the formal JSON
#' Schema shipped under \code{inst/schema/<schema_version>/}; validate a file
#' against it with \code{validate_graph_json()} or by passing
#' \code{validate = TRUE} when reading.
#'
#' @param graph A \code{dependency_graph} produced by
#'   \code{build_dependency_graph()} or \code{graph_from_metadata()}.
#' @param path Path to write to or read from.
#' @param pretty If \code{TRUE} (default), the JSON is indented for human
#'   inspection. Set \code{FALSE} for a compact representation.
#' @param validate If \code{TRUE}, check the file against the shipped schema
#'   (\code{validate_graph_json()} / \code{validate_split_spec_json()}) before
#'   parsing and run \code{validate_graph()} (for graphs) or
#'   \code{validate_split_spec()} (for specs) on the result, failing with a
#'   classed error on any violation or error-severity issue. The default
#'   \code{FALSE} loads the object as written, so a graph saved with
#'   \code{validate = FALSE} or predating a validation rule still loads; use
#'   \code{validate = TRUE} for files from untrusted or older sources.
#' @return \code{write_dependency_graph()} invisibly returns \code{path}.
#'   \code{read_dependency_graph()} returns a \code{dependency_graph} whose
#'   node and edge tables are checked for internal consistency with the
#'   rebuilt \code{igraph}; with the default \code{validate = FALSE} it is
#'   \emph{not} re-run through \code{validate_graph()}.
#' @examples
#' if (requireNamespace("jsonlite", quietly = TRUE)) {
#'   meta <- data.frame(
#'     sample_id  = c("S1", "S2"),
#'     subject_id = c("P1", "P2")
#'   )
#'   g <- graph_from_metadata(meta, graph_name = "demo")
#'
#'   tmp <- tempfile(fileext = ".json")
#'   write_dependency_graph(g, tmp)
#'   g2 <- read_dependency_graph(tmp)
#'   identical(g$nodes$data$node_id, g2$nodes$data$node_id)
#'   unlink(tmp)
#' }
#' @export
write_dependency_graph <- function(graph, path, pretty = TRUE) {
  .depgraph_require_jsonlite()
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path), "`path` must be a single non-empty file path.")
  .depgraph_check_writable_path(path)

  node_rows <- .depgraph_node_rows_to_json(graph$nodes$data)
  edge_rows <- .depgraph_edge_rows_to_json(graph$edges$data)

  payload <- list(
    `$schema`         = .depgraph_schema_url("dependency_graph"),
    splitGraph_object = "dependency_graph",
    schema_version    = .depgraph_schema_version,
    metadata          = .depgraph_metadata_to_json(graph$metadata),
    nodes             = node_rows,
    edges             = edge_rows
  )

  json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = isTRUE(pretty)
  )
  writeLines(json, path)
  invisible(path)
}

#' @rdname write_dependency_graph
#' @export
read_dependency_graph <- function(path, validate = FALSE) {
  .depgraph_require_jsonlite()
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path), "`path` must be a single non-empty file path.")
  .depgraph_assert(file.exists(path), paste0("File not found: ", path),
                   class = "splitgraph_io_error", code = "file_not_found")

  if (isTRUE(validate)) {
    .depgraph_require_json_conformance(validate_graph_json(path))
  }

  parsed <- .depgraph_parse_json_file(path)

  obj_type <- parsed$splitGraph_object %||% NA_character_
  if (!identical(as.character(obj_type), "dependency_graph")) {
    .depgraph_stop(
      c("JSON at `", path, "` is not a serialized dependency_graph (found `",
        obj_type, "`). Use `read_split_spec()` for split_spec files."),
      class = "splitgraph_schema_error", code = "unexpected_object_type"
    )
  }

  .depgraph_check_schema_version(parsed$schema_version, "dependency_graph")

  node_rows <- parsed$nodes %||% list()
  edge_rows <- parsed$edges %||% list()

  node_data <- if (length(node_rows) == 0L) {
    .depgraph_empty_node_data()
  } else {
    data.frame(
      node_id   = vapply(node_rows, function(r) as.character(r$node_id), character(1)),
      node_type = vapply(node_rows, function(r) as.character(r$node_type), character(1)),
      node_key  = vapply(node_rows, function(r) as.character(r$node_key), character(1)),
      label     = vapply(node_rows, function(r) as.character(r$label), character(1)),
      attrs     = I(lapply(node_rows, function(r) .depgraph_attrs_from_json(r$attrs))),
      stringsAsFactors = FALSE
    )
  }

  edge_data <- if (length(edge_rows) == 0L) {
    .depgraph_empty_edge_data()
  } else {
    data.frame(
      edge_id   = vapply(edge_rows, function(r) as.character(r$edge_id), character(1)),
      from      = vapply(edge_rows, function(r) as.character(r$from), character(1)),
      to        = vapply(edge_rows, function(r) as.character(r$to), character(1)),
      edge_type = vapply(edge_rows, function(r) as.character(r$edge_type), character(1)),
      attrs     = I(lapply(edge_rows, function(r) .depgraph_attrs_from_json(r$attrs))),
      stringsAsFactors = FALSE
    )
  }

  metadata <- .depgraph_metadata_from_json(parsed$metadata)

  graph <- dependency_graph(
    nodes    = graph_node_set(node_data),
    edges    = graph_edge_set(edge_data),
    graph    = NULL,
    metadata = metadata
  )

  if (isTRUE(validate)) {
    validate_graph(graph, error_on_fail = TRUE)
  }

  graph
}

# Turn a failed JSON conformance report into a classed error.
.depgraph_require_json_conformance <- function(report) {
  if (isTRUE(report$valid)) return(invisible(report))
  .depgraph_stop(
    c(
      "JSON at `", report$path, "` does not conform to the ", report$object_type,
      " schema (", report$schema, "):\n",
      paste0("  - ", report$issues, collapse = "\n")
    ),
    class = "splitgraph_schema_error",
    code = "json_schema_violation"
  )
}

# ---- Public API: split_spec -------------------------------------------------

# The canonical sample_data columns, in on-disk order.
.depgraph_split_spec_columns <- c(
  "sample_id", "sample_node_id", "group_id", "primary_group",
  "batch_group", "study_group", "site_group", "region_group",
  "platform_group", "assay_group", "stratum", "timepoint_id", "time_index", "order_rank"
)

# split_spec sample_data in canonical column order for the JSON writer. NA is
# preserved as null in the output stream (`na = "null"` in toJSON); columns
# absent from an older object are filled with NA so the on-disk shape is stable.
.depgraph_split_spec_rows_to_json <- function(sample_data) {
  if (nrow(sample_data) == 0L) return(list())
  columns <- lapply(.depgraph_split_spec_columns, function(col) {
    if (col %in% names(sample_data)) sample_data[[col]] else rep(NA, nrow(sample_data))
  })
  names(columns) <- .depgraph_split_spec_columns
  out <- as.data.frame(columns, stringsAsFactors = FALSE, optional = TRUE)
  row.names(out) <- NULL
  out
}

# Metadata fields that are character *vectors* by contract. `toJSON(auto_unbox
# = TRUE)` would collapse a length-1 vector to a bare string, which violates
# the shipped schema (`relations_used` is declared as an array) and makes the
# field's JSON type depend on its length. Wrapping in `as.list()` forces an
# array regardless of length; the reader unlists them back.
.depgraph_split_spec_vector_fields <- c("warnings", "relations_used", "enrichment_warnings", "via", "priority")

.depgraph_metadata_split_spec_to_json <- function(metadata) {
  # An empty (or NULL) metadata list must serialise as `{}`, not `[]`: the
  # schema declares an object, and an unnamed empty list would emit an array.
  # Reachable for a `split_spec()` built by hand rather than by
  # `as_split_spec()`, which always fills metadata.
  if (is.null(metadata) || length(metadata) == 0L) return(structure(list(), names = character()))
  out <- as.list(metadata)
  for (k in .depgraph_split_spec_vector_fields) {
    if (!is.null(out[[k]])) {
      out[[k]] <- as.list(as.character(out[[k]]))
    }
  }
  out
}

.depgraph_metadata_split_spec_from_json <- function(meta_list) {
  if (is.null(meta_list) || length(meta_list) == 0L) return(list())
  out <- as.list(meta_list)
  # warnings/relations_used/enrichment_warnings are character vectors;
  # jsonlite keeps them as lists when simplifyVector = FALSE — coerce back.
  for (k in .depgraph_split_spec_vector_fields) {
    if (!is.null(out[[k]])) {
      out[[k]] <- as.character(unlist(out[[k]]))
    }
  }
  # Scalar provenance fields written as `null` come back as NULL list
  # elements; restore the typed NA the writer started from so a spec
  # round-trips its metadata exactly.
  if ("threshold" %in% names(out) && is.null(out$threshold)) out$threshold <- NA_real_
  if ("threshold_metric" %in% names(out) && is.null(out$threshold_metric)) out$threshold_metric <- NA_character_
  out
}

#' Serialize a Split Specification to JSON
#'
#' Write a \code{split_spec} to a JSON file and read it back. The on-disk
#' format captures the canonical sample-level table (\code{sample_data}) plus
#' all spec-level fields needed by a downstream resampling adapter
#' (\code{group_var}, \code{block_vars}, \code{time_var},
#' \code{ordering_required}, \code{constraint_mode},
#' \code{constraint_strategy}, \code{recommended_resampling}) and the spec
#' metadata.
#'
#' \code{NA} values in \code{sample_data} are written as JSON \code{null} and
#' read back as \code{NA}.
#'
#' @section JSON format:
#' \preformatted{
#' {
#'   "$schema": "https://.../inst/schema/0.3.0/split_spec.schema.json",
#'   "splitGraph_object": "split_spec",
#'   "schema_version": "0.3.0",
#'   "group_var": "group_id",
#'   "block_vars": ["batch_group", "study_group"],
#'   "time_var": "order_rank",
#'   "stratum_var": "stratum",
#'   "ordering_required": false,
#'   "constraint_mode": "subject",
#'   "constraint_strategy": "subject",
#'   "recommended_resampling": "grouped_cv",
#'   "metadata": { "relations_used": [...], "via": [...], "priority": [...],
#'                 "threshold": null, "warnings": [...], ... },
#'   "sample_data": [
#'     { "sample_id": "S1", "group_id": "subject:P1", "stratum": "case", ... },
#'     ...
#'   ]
#' }
#' }
#' Vector-valued metadata fields are always written as arrays, even with a
#' single element. \code{stratum_var} and the \code{stratum} column were added
#' in schema 0.3.0; files written by earlier versions load with \code{stratum}
#' filled as \code{NA}.
#'
#' @param spec A \code{split_spec} produced by \code{as_split_spec()}.
#' @param path Path to write to or read from.
#' @param pretty If \code{TRUE} (default), the JSON is indented.
#' @param validate If \code{TRUE}, check the file against the shipped schema
#'   before parsing and run \code{validate_split_spec()} on the result, failing
#'   with a classed error on any violation or error-severity issue. Defaults to
#'   \code{FALSE}.
#' @return \code{write_split_spec()} invisibly returns \code{path}.
#'   \code{read_split_spec()} returns a \code{split_spec}.
#' @examples
#' if (requireNamespace("jsonlite", quietly = TRUE)) {
#'   meta <- data.frame(
#'     sample_id  = c("S1", "S2"),
#'     subject_id = c("P1", "P2")
#'   )
#'   g <- graph_from_metadata(meta)
#'   constraint <- derive_split_constraints(g, mode = "subject")
#'   spec <- as_split_spec(constraint, graph = g)
#'
#'   tmp <- tempfile(fileext = ".json")
#'   write_split_spec(spec, tmp)
#'   spec2 <- read_split_spec(tmp)
#'   identical(spec$sample_data$group_id, spec2$sample_data$group_id)
#'   unlink(tmp)
#' }
#' @export
write_split_spec <- function(spec, path, pretty = TRUE) {
  .depgraph_require_jsonlite()
  .depgraph_assert(inherits(spec, "split_spec"), "`spec` must be a `split_spec`.")
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path), "`path` must be a single non-empty file path.")
  .depgraph_check_writable_path(path)

  sample_rows <- .depgraph_split_spec_rows_to_json(spec$sample_data)

  payload <- list(
    `$schema`               = .depgraph_schema_url("split_spec"),
    splitGraph_object       = "split_spec",
    schema_version          = .depgraph_schema_version,
    group_var               = spec$group_var,
    block_vars              = if (length(spec$block_vars) == 0L) list() else as.list(spec$block_vars),
    time_var                = spec$time_var %||% NA_character_,
    stratum_var             = spec$stratum_var %||% NA_character_,
    ordering_required       = isTRUE(spec$ordering_required),
    constraint_mode         = spec$constraint_mode %||% NA_character_,
    constraint_strategy     = spec$constraint_strategy %||% NA_character_,
    recommended_resampling  = spec$recommended_resampling %||% NA_character_,
    metadata                = .depgraph_metadata_split_spec_to_json(spec$metadata),
    sample_data             = sample_rows
  )

  json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = isTRUE(pretty)
  )
  writeLines(json, path)
  invisible(path)
}

#' @rdname write_split_spec
#' @export
read_split_spec <- function(path, validate = FALSE) {
  .depgraph_require_jsonlite()
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path), "`path` must be a single non-empty file path.")
  .depgraph_assert(file.exists(path), paste0("File not found: ", path),
                   class = "splitgraph_io_error", code = "file_not_found")

  if (isTRUE(validate)) {
    .depgraph_require_json_conformance(validate_split_spec_json(path))
  }

  parsed <- .depgraph_parse_json_file(path)

  obj_type <- parsed$splitGraph_object %||% NA_character_
  if (!identical(as.character(obj_type), "split_spec")) {
    .depgraph_stop(
      c("JSON at `", path, "` is not a serialized split_spec (found `",
        obj_type, "`). Use `read_dependency_graph()` for dependency_graph files."),
      class = "splitgraph_schema_error", code = "unexpected_object_type"
    )
  }

  .depgraph_check_schema_version(parsed$schema_version, "split_spec")

  sample_rows <- parsed$sample_data %||% list()
  sample_data <- if (length(sample_rows) == 0L) {
    .split_spec_sample_data_template(0L)
  } else {
    .depgraph_chr <- function(rows, key) {
      vapply(rows, function(r) {
        v <- r[[key]]
        if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_ else as.character(v)
      }, character(1))
    }
    .depgraph_num <- function(rows, key) {
      vapply(rows, function(r) {
        v <- r[[key]]
        if (is.null(v) || (length(v) == 1L && is.na(v))) NA_real_ else as.numeric(v)
      }, numeric(1))
    }
    .depgraph_int <- function(rows, key) {
      vapply(rows, function(r) {
        v <- r[[key]]
        if (is.null(v) || (length(v) == 1L && is.na(v))) NA_integer_ else as.integer(v)
      }, integer(1))
    }

    data.frame(
      sample_id      = .depgraph_chr(sample_rows, "sample_id"),
      sample_node_id = .depgraph_chr(sample_rows, "sample_node_id"),
      group_id       = .depgraph_chr(sample_rows, "group_id"),
      primary_group  = .depgraph_chr(sample_rows, "primary_group"),
      batch_group    = .depgraph_chr(sample_rows, "batch_group"),
      study_group    = .depgraph_chr(sample_rows, "study_group"),
      site_group     = .depgraph_chr(sample_rows, "site_group"),
      region_group   = .depgraph_chr(sample_rows, "region_group"),
      platform_group = .depgraph_chr(sample_rows, "platform_group"),
      assay_group    = .depgraph_chr(sample_rows, "assay_group"),
      stratum        = .depgraph_chr(sample_rows, "stratum"),
      timepoint_id   = .depgraph_chr(sample_rows, "timepoint_id"),
      time_index     = .depgraph_num(sample_rows, "time_index"),
      order_rank     = .depgraph_int(sample_rows, "order_rank"),
      stringsAsFactors = FALSE
    )
  }

  block_vars_raw <- parsed$block_vars
  block_vars <- if (is.null(block_vars_raw)) {
    character()
  } else if (length(block_vars_raw) == 0L) {
    character()
  } else {
    as.character(unlist(block_vars_raw))
  }

  time_var_raw <- parsed$time_var
  time_var <- if (is.null(time_var_raw) || (length(time_var_raw) == 1L && is.na(time_var_raw))) NULL else as.character(time_var_raw)
  stratum_var_raw <- parsed$stratum_var
  stratum_var <- if (is.null(stratum_var_raw) || (length(stratum_var_raw) == 1L && is.na(stratum_var_raw))) NULL else as.character(stratum_var_raw)
  constraint_mode <- if (is.null(parsed$constraint_mode) || is.na(parsed$constraint_mode)) NULL else as.character(parsed$constraint_mode)
  constraint_strategy <- if (is.null(parsed$constraint_strategy) || is.na(parsed$constraint_strategy)) NULL else as.character(parsed$constraint_strategy)
  recommended_resampling <- if (is.null(parsed$recommended_resampling) ||
                                  is.na(parsed$recommended_resampling)) {
    NULL
  } else {
    as.character(parsed$recommended_resampling)
  }

  spec <- split_spec(
    sample_data            = sample_data,
    group_var              = parsed$group_var %||% "group_id",
    block_vars             = block_vars,
    time_var               = time_var,
    stratum_var            = stratum_var,
    ordering_required      = isTRUE(parsed$ordering_required),
    constraint_mode        = constraint_mode,
    constraint_strategy    = constraint_strategy,
    recommended_resampling = recommended_resampling,
    metadata               = .depgraph_metadata_split_spec_from_json(parsed$metadata)
  )

  if (isTRUE(validate)) {
    preflight <- validate_split_spec(spec)
    if (!isTRUE(preflight$valid)) {
      .depgraph_stop(
        c(
          "split_spec read from `", path, "` fails preflight validation:\n",
          paste0("  - ", preflight$issues$message[preflight$issues$severity == "error"], collapse = "\n")
        ),
        class = "splitgraph_validation_error",
        code = "split_spec_validation_failed"
      )
    }
  }

  spec
}

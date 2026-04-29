# JSON serialization for splitGraph's two handoff objects:
# `dependency_graph` and `split_spec`. The on-disk format is documented in
# the @section JSON format blocks below and is intended to be stable across
# patch releases. Schema bumps follow the rules described in
# `R/splitGraph-package.R` (`.depgraph_schema_version`).

.depgraph_require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required for splitGraph JSON serialization. ",
      "Install it with: install.packages(\"jsonlite\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.depgraph_check_writable_path <- function(path) {
  parent <- dirname(path)
  if (!nzchar(parent) || identical(parent, ".")) return(invisible())
  if (!dir.exists(parent)) {
    stop(
      "Parent directory does not exist: ", parent,
      ". Create it before writing.",
      call. = FALSE
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
  if (is.null(x) || identical(x, NA_character_) || (length(x) == 1L && is.na(x))) {
    return(Sys.time())
  }
  parsed <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS%z"))
  if (length(parsed) == 0L || is.na(parsed)) {
    parsed <- suppressWarnings(as.POSIXct(x))
  }
  if (is.na(parsed)) Sys.time() else parsed
}

.depgraph_check_schema_version <- function(observed, what) {
  if (is.null(observed) || !nzchar(observed)) {
    warning(
      "Reading ", what, ": no `schema_version` recorded in JSON. ",
      "Assuming current schema (", .depgraph_schema_version, ").",
      call. = FALSE
    )
    return(invisible())
  }
  if (!identical(as.character(observed), .depgraph_schema_version)) {
    warning(
      "Reading ", what, ": JSON schema_version `", observed,
      "` does not match installed splitGraph schema_version `",
      .depgraph_schema_version, "`. Loading anyway.",
      call. = FALSE
    )
  }
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

.depgraph_node_row_to_list <- function(row) {
  list(
    node_id   = row$node_id,
    node_type = row$node_type,
    node_key  = row$node_key,
    label     = row$label,
    attrs     = .depgraph_attrs_to_json(row$attrs[[1L]])
  )
}

.depgraph_edge_row_to_list <- function(row) {
  list(
    edge_id   = row$edge_id,
    from      = row$from,
    to        = row$to,
    edge_type = row$edge_type,
    attrs     = .depgraph_attrs_to_json(row$attrs[[1L]])
  )
}

# Serialize graph metadata, dropping fields that are not portable
# (validation_overrides IS preserved; igraph object is not stored).
.depgraph_metadata_to_json <- function(metadata) {
  out <- list(
    graph_name           = metadata$graph_name %||% NA_character_,
    dataset_name         = metadata$dataset_name %||% NA_character_,
    created_at           = .depgraph_posix_to_iso(metadata$created_at),
    schema_version       = metadata$schema_version %||% .depgraph_schema_version,
    validation_overrides = if (is.null(metadata$validation_overrides)) {
      structure(list(), names = character())
    } else {
      as.list(metadata$validation_overrides)
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
#'   "splitGraph_object": "dependency_graph",
#'   "schema_version": "0.1.0",
#'   "metadata": {
#'     "graph_name": "...",
#'     "dataset_name": "...",
#'     "created_at": "2026-04-29T10:11:12.000000+0000",
#'     "schema_version": "0.1.0",
#'     "validation_overrides": { ... }
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
#' Reading a file whose \code{schema_version} does not match the installed
#' package emits a warning but still loads.
#'
#' @param graph A \code{dependency_graph} produced by
#'   \code{build_dependency_graph()} or \code{graph_from_metadata()}.
#' @param path Path to write to or read from.
#' @param pretty If \code{TRUE} (default), the JSON is indented for human
#'   inspection. Set \code{FALSE} for a compact representation.
#' @return \code{write_dependency_graph()} invisibly returns \code{path}.
#'   \code{read_dependency_graph()} returns a validated
#'   \code{dependency_graph}.
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

  node_rows <- if (nrow(graph$nodes$data) == 0L) list() else lapply(
    seq_len(nrow(graph$nodes$data)),
    function(i) .depgraph_node_row_to_list(graph$nodes$data[i, , drop = FALSE])
  )
  edge_rows <- if (nrow(graph$edges$data) == 0L) list() else lapply(
    seq_len(nrow(graph$edges$data)),
    function(i) .depgraph_edge_row_to_list(graph$edges$data[i, , drop = FALSE])
  )

  payload <- list(
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
read_dependency_graph <- function(path) {
  .depgraph_require_jsonlite()
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path), "`path` must be a single non-empty file path.")
  .depgraph_assert(file.exists(path), paste0("File not found: ", path))

  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse JSON at `", path, "`: ", conditionMessage(e), call. = FALSE)
    }
  )

  obj_type <- parsed$splitGraph_object %||% NA_character_
  if (!identical(as.character(obj_type), "dependency_graph")) {
    stop(
      "JSON at `", path, "` is not a serialized dependency_graph (found `",
      obj_type, "`). Use `read_split_spec()` for split_spec files.",
      call. = FALSE
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

  dependency_graph(
    nodes    = graph_node_set(node_data),
    edges    = graph_edge_set(edge_data),
    graph    = NULL,
    metadata = metadata
  )
}

# ---- Public API: split_spec -------------------------------------------------

# Convert a split_spec's sample_data row to a JSON-friendly list, preserving
# NA as null in the output stream (na = "null" in toJSON does this for us).
.depgraph_split_spec_row_to_list <- function(row) {
  list(
    sample_id      = row$sample_id,
    sample_node_id = row$sample_node_id,
    group_id       = row$group_id,
    primary_group  = row$primary_group,
    batch_group    = row$batch_group,
    study_group    = row$study_group,
    timepoint_id   = row$timepoint_id,
    time_index     = row$time_index,
    order_rank     = row$order_rank
  )
}

.depgraph_metadata_split_spec_to_json <- function(metadata) {
  if (is.null(metadata)) return(structure(list(), names = character()))
  out <- as.list(metadata)
  out
}

.depgraph_metadata_split_spec_from_json <- function(meta_list) {
  if (is.null(meta_list) || length(meta_list) == 0L) return(list())
  out <- as.list(meta_list)
  # warnings/relations_used are character vectors; jsonlite keeps them as
  # lists when simplifyVector = FALSE — coerce back.
  for (k in c("warnings", "relations_used")) {
    if (!is.null(out[[k]])) {
      out[[k]] <- as.character(unlist(out[[k]]))
    }
  }
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
#'   "splitGraph_object": "split_spec",
#'   "schema_version": "0.1.0",
#'   "group_var": "group_id",
#'   "block_vars": ["batch_group", "study_group"],
#'   "time_var": "order_rank",
#'   "ordering_required": false,
#'   "constraint_mode": "subject",
#'   "constraint_strategy": "subject",
#'   "recommended_resampling": "grouped_cv",
#'   "metadata": { ... },
#'   "sample_data": [
#'     { "sample_id": "S1", "group_id": "subject:P1", ... },
#'     ...
#'   ]
#' }
#' }
#'
#' @param spec A \code{split_spec} produced by \code{as_split_spec()}.
#' @param path Path to write to or read from.
#' @param pretty If \code{TRUE} (default), the JSON is indented.
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

  sample_rows <- if (nrow(spec$sample_data) == 0L) list() else lapply(
    seq_len(nrow(spec$sample_data)),
    function(i) .depgraph_split_spec_row_to_list(spec$sample_data[i, , drop = FALSE])
  )

  payload <- list(
    splitGraph_object       = "split_spec",
    schema_version          = .depgraph_schema_version,
    group_var               = spec$group_var,
    block_vars              = if (length(spec$block_vars) == 0L) list() else as.list(spec$block_vars),
    time_var                = spec$time_var %||% NA_character_,
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
read_split_spec <- function(path) {
  .depgraph_require_jsonlite()
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path), "`path` must be a single non-empty file path.")
  .depgraph_assert(file.exists(path), paste0("File not found: ", path))

  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse JSON at `", path, "`: ", conditionMessage(e), call. = FALSE)
    }
  )

  obj_type <- parsed$splitGraph_object %||% NA_character_
  if (!identical(as.character(obj_type), "split_spec")) {
    stop(
      "JSON at `", path, "` is not a serialized split_spec (found `",
      obj_type, "`). Use `read_dependency_graph()` for dependency_graph files.",
      call. = FALSE
    )
  }

  .depgraph_check_schema_version(parsed$schema_version, "split_spec")

  sample_rows <- parsed$sample_data %||% list()
  sample_data <- if (length(sample_rows) == 0L) {
    data.frame(
      sample_id      = character(),
      sample_node_id = character(),
      group_id       = character(),
      primary_group  = character(),
      batch_group    = character(),
      study_group    = character(),
      timepoint_id   = character(),
      time_index     = numeric(),
      order_rank     = integer(),
      stringsAsFactors = FALSE
    )
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
  constraint_mode <- if (is.null(parsed$constraint_mode) || is.na(parsed$constraint_mode)) NULL else as.character(parsed$constraint_mode)
  constraint_strategy <- if (is.null(parsed$constraint_strategy) || is.na(parsed$constraint_strategy)) NULL else as.character(parsed$constraint_strategy)
  recommended_resampling <- if (is.null(parsed$recommended_resampling) || is.na(parsed$recommended_resampling)) NULL else as.character(parsed$recommended_resampling)

  split_spec(
    sample_data            = sample_data,
    group_var              = parsed$group_var %||% "group_id",
    block_vars             = block_vars,
    time_var               = time_var,
    ordering_required      = isTRUE(parsed$ordering_required),
    constraint_mode        = constraint_mode,
    constraint_strategy    = constraint_strategy,
    recommended_resampling = recommended_resampling,
    metadata               = .depgraph_metadata_split_spec_from_json(parsed$metadata)
  )
}

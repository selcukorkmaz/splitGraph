# Validation of serialized splitGraph JSON against the shipped contract, plus a
# one-shot upgrader for files written under an older schema_version. The formal
# JSON Schemas live in `inst/schema/` (Draft 2020-12) and are the portable
# contract for non-R consumers; the checks below are a dependency-free R
# encoding of the same structural invariants so `read_*()` callers can validate
# a handoff file without pulling in a JSON Schema engine.

# Path to a shipped schema file within the installed package (or source tree
# under pkgload). Returns "" if it cannot be located.
.depgraph_schema_path <- function(object_type) {
  file <- paste0(object_type, ".schema.json")
  p <- system.file("schema", file, package = "splitGraph")
  if (nzchar(p)) p else ""
}

.depgraph_json_report <- function(object_type, path, issues) {
  structure(
    list(
      object_type = object_type,
      path = path,
      valid = length(issues) == 0L,
      issues = issues,
      schema = .depgraph_schema_url(object_type)
    ),
    class = "splitgraph_json_report"
  )
}

#' @export
print.splitgraph_json_report <- function(x, ...) {
  cat("<splitGraph JSON validation>\n")
  cat("  object:  ", x$object_type, "\n", sep = "")
  cat("  path:    ", x$path, "\n", sep = "")
  cat("  valid:   ", x$valid, "\n", sep = "")
  if (!x$valid) {
    cat("  issues:\n")
    for (msg in x$issues) cat("    - ", msg, "\n", sep = "")
  }
  invisible(x)
}

.depgraph_parse_json_file <- function(path) {
  .depgraph_require_jsonlite()
  .depgraph_assert(is.character(path) && length(path) == 1L && nzchar(path),
                   "`path` must be a single non-empty file path.")
  .depgraph_assert(file.exists(path), paste0("File not found: ", path))
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse JSON at `", path, "`: ", conditionMessage(e), call. = FALSE)
    }
  )
}

.depgraph_is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

.depgraph_valid_version <- function(x) {
  .depgraph_is_string(x) && grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", x)
}

#' Validate Serialized splitGraph JSON Against the Shipped Schema
#'
#' Check that a JSON file written by \code{write_dependency_graph()} or
#' \code{write_split_spec()} conforms to the splitGraph on-disk contract. The
#' formal JSON Schemas (Draft 2020-12) ship in \code{inst/schema/} and are
#' referenced from the written JSON via the \code{$schema} key; these functions
#' apply a dependency-free structural check of the same invariants (required
#' fields, value types, node/edge-type enumerations, and referential integrity
#' of edge endpoints) so a handoff file can be validated without a JSON Schema
#' engine.
#'
#' @param path Path to a serialized \code{dependency_graph} or
#'   \code{split_spec} JSON file.
#' @return A \code{splitgraph_json_report}: a list with \code{valid} (logical),
#'   \code{issues} (character vector of failures), the detected
#'   \code{object_type}, and the schema \code{$id}.
#' @examples
#' if (requireNamespace("jsonlite", quietly = TRUE)) {
#'   meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"))
#'   g <- graph_from_metadata(meta)
#'   tmp <- tempfile(fileext = ".json")
#'   write_dependency_graph(g, tmp)
#'   validate_graph_json(tmp)
#'   unlink(tmp)
#' }
#' @name validate_json
#' @export
validate_graph_json <- function(path) {
  parsed <- .depgraph_parse_json_file(path)
  issues <- character()

  if (!identical(as.character(parsed$splitGraph_object %||% NA_character_), "dependency_graph")) {
    issues <- c(issues, paste0(
      "`splitGraph_object` must be \"dependency_graph\" (found \"",
      parsed$splitGraph_object %||% "<missing>", "\")."
    ))
    return(.depgraph_json_report("dependency_graph", path, issues))
  }

  if (!.depgraph_valid_version(parsed$schema_version)) {
    issues <- c(issues, "`schema_version` must be a \"X.Y.Z\" string.")
  }

  node_rows <- parsed$nodes
  edge_rows <- parsed$edges
  if (!is.null(node_rows) && !is.list(node_rows)) {
    issues <- c(issues, "`nodes` must be an array.")
    node_rows <- list()
  }
  if (!is.null(edge_rows) && !is.list(edge_rows)) {
    issues <- c(issues, "`edges` must be an array.")
    edge_rows <- list()
  }

  node_ids <- character()
  for (i in seq_along(node_rows)) {
    r <- node_rows[[i]]
    if (!.depgraph_is_string(r$node_id) || !.depgraph_is_string(r$node_type) ||
        !.depgraph_is_string(r$node_key)) {
      issues <- c(issues, paste0("node[", i, "]: `node_id`, `node_type`, `node_key` are required strings."))
      next
    }
    node_ids <- c(node_ids, r$node_id)
    if (!r$node_type %in% .depgraph_node_types) {
      issues <- c(issues, paste0("node[", i, "]: unknown `node_type` \"", r$node_type, "\"."))
    }
  }

  valid_edge_types <- .depgraph_edge_schema$edge_type
  for (i in seq_along(edge_rows)) {
    r <- edge_rows[[i]]
    if (!.depgraph_is_string(r$edge_id) || !.depgraph_is_string(r$from) ||
        !.depgraph_is_string(r$to) || !.depgraph_is_string(r$edge_type)) {
      issues <- c(issues, paste0("edge[", i, "]: `edge_id`, `from`, `to`, `edge_type` are required strings."))
      next
    }
    if (!r$edge_type %in% valid_edge_types) {
      issues <- c(issues, paste0("edge[", i, "]: unknown `edge_type` \"", r$edge_type, "\"."))
    }
    if (length(node_ids) > 0L) {
      if (!r$from %in% node_ids) {
        issues <- c(issues, paste0("edge[", i, "]: `from` \"", r$from, "\" does not reference a declared node."))
      }
      if (!r$to %in% node_ids) {
        issues <- c(issues, paste0("edge[", i, "]: `to` \"", r$to, "\" does not reference a declared node."))
      }
    }
  }

  .depgraph_json_report("dependency_graph", path, issues)
}

#' @rdname validate_json
#' @export
validate_split_spec_json <- function(path) {
  parsed <- .depgraph_parse_json_file(path)
  issues <- character()

  if (!identical(as.character(parsed$splitGraph_object %||% NA_character_), "split_spec")) {
    issues <- c(issues, paste0(
      "`splitGraph_object` must be \"split_spec\" (found \"",
      parsed$splitGraph_object %||% "<missing>", "\")."
    ))
    return(.depgraph_json_report("split_spec", path, issues))
  }

  if (!.depgraph_valid_version(parsed$schema_version)) {
    issues <- c(issues, "`schema_version` must be a \"X.Y.Z\" string.")
  }
  if (!.depgraph_is_string(parsed$group_var)) {
    issues <- c(issues, "`group_var` must be a string.")
  }
  if (!is.null(parsed$block_vars) && !is.list(parsed$block_vars)) {
    issues <- c(issues, "`block_vars` must be an array of strings.")
  }

  sample_rows <- parsed$sample_data
  if (is.null(sample_rows) || !is.list(sample_rows)) {
    issues <- c(issues, "`sample_data` must be an array.")
    sample_rows <- list()
  }
  for (i in seq_along(sample_rows)) {
    r <- sample_rows[[i]]
    if (!.depgraph_is_string(r$sample_id)) {
      issues <- c(issues, paste0("sample_data[", i, "]: `sample_id` is a required string."))
    }
    if (!.depgraph_is_string(r$group_id)) {
      issues <- c(issues, paste0("sample_data[", i, "]: `group_id` is a required string."))
    }
  }

  .depgraph_json_report("split_spec", path, issues)
}

#' Upgrade Serialized splitGraph JSON to the Current Schema Version
#'
#' Read a \code{dependency_graph} or \code{split_spec} JSON file written under
#' an older \code{schema_version} and rewrite it at the installed version. The
#' round-trip fills any field introduced since the file was written with its
#' default (\code{NA} for missing \code{sample_data} columns), stamps the
#' current \code{schema_version}, and adds the \code{$schema} reference. Files
#' already at the current version are rewritten unchanged.
#'
#' @param path Path to the JSON file to upgrade.
#' @param out Path to write the upgraded file to. Defaults to \code{path}
#'   (in-place upgrade).
#' @return The output path, invisibly.
#' @examples
#' if (requireNamespace("jsonlite", quietly = TRUE)) {
#'   meta <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"))
#'   g <- graph_from_metadata(meta)
#'   tmp <- tempfile(fileext = ".json")
#'   write_dependency_graph(g, tmp)
#'   migrate_dependency_graph_json(tmp)
#'   unlink(tmp)
#' }
#' @name migrate_json
#' @export
migrate_dependency_graph_json <- function(path, out = path) {
  g <- read_dependency_graph(path)
  write_dependency_graph(g, out)
  invisible(out)
}

#' @rdname migrate_json
#' @export
migrate_split_spec_json <- function(path, out = path) {
  spec <- read_split_spec(path)
  write_split_spec(spec, out)
  invisible(out)
}

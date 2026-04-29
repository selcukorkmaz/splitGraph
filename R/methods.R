# S3 methods for splitGraph core classes.

#' @export
print.graph_node_set <- function(x, ...) {
  cat(
    "<graph_node_set>",
    nrow(x$data), "nodes across",
    length(unique(x$data$node_type)), "types",
    "(schema", x$schema_version, ")\n"
  )
  invisible(x)
}

#' @export
summary.graph_node_set <- function(object, ...) {
  list(
    schema_version = object$schema_version,
    n_nodes = nrow(object$data),
    node_types = .depgraph_count_table(object$data$node_type)
  )
}

#' @export
as.data.frame.graph_node_set <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$data
}

#' @export
print.graph_edge_set <- function(x, ...) {
  cat(
    "<graph_edge_set>",
    nrow(x$data), "edges across",
    length(unique(x$data$edge_type)), "relations",
    "(schema", x$schema_version, ")\n"
  )
  invisible(x)
}

#' @export
summary.graph_edge_set <- function(object, ...) {
  list(
    schema_version = object$schema_version,
    n_edges = nrow(object$data),
    edge_types = .depgraph_count_table(object$data$edge_type)
  )
}

#' @export
as.data.frame.graph_edge_set <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$data
}

#' @export
print.dependency_graph <- function(x, ...) {
  graph_name <- x$metadata$graph_name %||% "<unnamed>"
  cat(
    "<dependency_graph>",
    graph_name,
    "\n  Nodes:", igraph::vcount(x$graph),
    "\n  Edges:", igraph::ecount(x$graph),
    "\n"
  )
  invisible(x)
}

#' @export
summary.dependency_graph <- function(object, ...) {
  list(
    graph_name = object$metadata$graph_name,
    dataset_name = object$metadata$dataset_name,
    schema_version = object$metadata$schema_version,
    n_nodes = nrow(object$nodes$data),
    n_edges = nrow(object$edges$data),
    node_types = .depgraph_count_table(object$nodes$data$node_type),
    edge_types = .depgraph_count_table(object$edges$data$edge_type)
  )
}

.depgraph_type_rows <- c(
  Sample     = 0L,
  Subject    = 1L,
  Batch      = 1L,
  Study      = 1L,
  Timepoint  = 1L,
  Assay      = 2L,
  FeatureSet = 2L,
  Outcome    = 3L
)

.depgraph_type_palette <- function() {
  c(
    Sample     = "#4C78A8",
    Subject    = "#F58518",
    Batch      = "#54A24B",
    Study      = "#B279A2",
    Timepoint  = "#E45756",
    Assay      = "#72B7B2",
    FeatureSet = "#EECA3B",
    Outcome    = "#9D755D",
    `_other_`  = "#B0B0B0"
  )
}

.depgraph_typed_layout <- function(graph) {
  node_types <- igraph::vertex_attr(graph, "node_type")
  if (is.null(node_types)) {
    node_types <- rep("Sample", igraph::vcount(graph))
  }

  rows <- .depgraph_type_rows[node_types]
  rows[is.na(rows)] <- max(.depgraph_type_rows) + 1L

  order_in_row <- stats::ave(seq_along(rows), rows, FUN = seq_along)
  row_widths <- stats::ave(seq_along(rows), rows, FUN = length)

  x <- ifelse(row_widths > 1L,
              (order_in_row - 1L) / (row_widths - 1L) - 0.5,
              0)
  y <- -as.numeric(rows)

  cbind(x, y)
}

.depgraph_node_colors <- function(graph, node_colors = NULL) {
  palette <- .depgraph_type_palette()
  if (!is.null(node_colors)) {
    for (nm in names(node_colors)) palette[[nm]] <- node_colors[[nm]]
  }
  node_types <- igraph::vertex_attr(graph, "node_type")
  if (is.null(node_types)) {
    node_types <- rep("Sample", igraph::vcount(graph))
  }
  colors <- palette[node_types]
  colors[is.na(colors)] <- palette[["_other_"]]
  unname(colors)
}

#' @export
plot.dependency_graph <- function(x,
                                  layout = c("typed", "sugiyama", "auto"),
                                  node_colors = NULL,
                                  show_labels = TRUE,
                                  legend = TRUE,
                                  legend_position = "topleft",
                                  ...) {
  layout_choice <- if (is.character(layout)) match.arg(layout) else layout
  g <- x$graph

  user_args <- list(...)
  plot_args <- list(x = g)

  if (is.character(layout_choice)) {
    if (identical(layout_choice, "typed")) {
      plot_args$layout <- .depgraph_typed_layout(g)
    } else if (identical(layout_choice, "sugiyama")) {
      plot_args$layout <- igraph::layout_with_sugiyama(g)$layout
    }
  } else if (is.matrix(layout_choice)) {
    plot_args$layout <- layout_choice
  } else if (is.function(layout_choice)) {
    plot_args$layout <- layout_choice(g)
  }

  if (is.null(user_args[["vertex.color"]])) {
    plot_args$vertex.color <- .depgraph_node_colors(g, node_colors = node_colors)
  }
  if (is.null(user_args[["vertex.label"]])) {
    plot_args$vertex.label <- if (isTRUE(show_labels)) {
      lbl <- igraph::vertex_attr(g, "label")
      if (is.null(lbl) || all(is.na(lbl) | !nzchar(lbl))) {
        igraph::vertex_attr(g, "name")
      } else {
        ifelse(is.na(lbl) | !nzchar(lbl), igraph::vertex_attr(g, "name"), lbl)
      }
    } else {
      NA
    }
  }
  if (is.null(user_args[["vertex.label.cex"]])) plot_args$vertex.label.cex <- 0.7
  if (is.null(user_args[["vertex.size"]]))       plot_args$vertex.size <- 14
  if (is.null(user_args[["edge.arrow.size"]]))   plot_args$edge.arrow.size <- 0.4
  if (is.null(user_args[["edge.color"]]))        plot_args$edge.color <- "#888888"

  plot_args[names(user_args)] <- user_args

  do.call(graphics::plot, plot_args)

  if (isTRUE(legend)) {
    palette <- .depgraph_type_palette()
    if (!is.null(node_colors)) {
      for (nm in names(node_colors)) palette[[nm]] <- node_colors[[nm]]
    }
    node_types <- igraph::vertex_attr(g, "node_type")
    present <- intersect(names(palette), unique(node_types))
    if (length(present) > 0L) {
      graphics::legend(
        legend_position,
        legend = present,
        pt.bg = unname(palette[present]),
        pch = 21,
        pt.cex = 1.4,
        col = "#333333",
        bty = "n",
        cex = 0.75,
        title = "Node type"
      )
    }
  }

  invisible(x)
}

#' @export
print.graph_query_result <- function(x, ...) {
  cat("<graph_query_result>", x$query, "\n")
  cat("  Rows:", nrow(x$table), "\n")
  invisible(x)
}

#' @export
summary.graph_query_result <- function(object, ...) {
  list(
    query = object$query,
    n_nodes = nrow(object$nodes),
    n_edges = nrow(object$edges),
    n_rows = nrow(object$table)
  )
}

#' @export
as.data.frame.graph_query_result <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$table
}

#' @export
print.dependency_constraint <- function(x, ...) {
  cat("<dependency_constraint>", x$constraint_id, "\n")
  cat("  Samples:", nrow(x$sample_map), "\n")
  invisible(x)
}

#' @export
summary.dependency_constraint <- function(object, ...) {
  group_col <- intersect(c("group_id", "split_unit", "block_id"), names(object$sample_map))
  n_groups <- if (length(group_col) == 0L) NA_integer_ else length(unique(object$sample_map[[group_col[[1L]]]]))
  list(
    constraint_id = object$constraint_id,
    relation_types = object$relation_types,
    n_samples = nrow(object$sample_map),
    n_groups = n_groups,
    transitive = object$transitive
  )
}

#' @export
as.data.frame.dependency_constraint <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$sample_map
}

#' @export
print.split_constraint <- function(x, ...) {
  cat("<split_constraint>", x$strategy, "\n")
  cat("  Samples:", nrow(x$sample_map), "\n")
  if ("group_id" %in% names(x$sample_map)) {
    cat("  Groups:", length(unique(x$sample_map$group_id)), "\n")
  }
  warnings <- x$metadata$warnings %||% character()
  if (length(warnings) > 0L) {
    cat("  Warnings:", length(warnings), "\n")
  }
  invisible(x)
}

#' @export
summary.split_constraint <- function(object, ...) {
  warnings <- object$metadata$warnings %||% character()
  list(
    strategy = object$strategy,
    mode = object$metadata$mode %||% NA_character_,
    n_samples = nrow(object$sample_map),
    n_groups = if ("group_id" %in% names(object$sample_map)) length(unique(object$sample_map$group_id)) else NA_integer_,
    n_warnings = length(warnings),
    columns = names(object$sample_map)
  )
}

#' @export
as.data.frame.split_constraint <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$sample_map
}

#' @export
print.leakage_constraint <- function(x, ...) {
  cat("<leakage_constraint>", x$issue_type, "(", x$severity, ")\n")
  cat("  Affected samples:", length(x$affected_samples), "\n")
  invisible(x)
}

#' @export
summary.leakage_constraint <- function(object, ...) {
  list(
    issue_type = object$issue_type,
    severity = object$severity,
    n_affected_samples = length(object$affected_samples),
    recommendation = object$recommendation
  )
}

#' @export
as.data.frame.leakage_constraint <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$evidence
}

#' @export
print.depgraph_validation_report <- function(x, ...) {
  cat("<depgraph_validation_report>")
  if (!is.null(x$graph_name) && nzchar(x$graph_name)) {
    cat(" ", x$graph_name)
  }
  cat(
    "\n  Valid:", x$valid,
    "\n  Issues:", nrow(x$issues),
    "\n"
  )

  severity_counts <- x$summary$by_severity
  if (nrow(severity_counts) > 0L) {
    cat("  By severity:\n")
    for (i in seq_len(nrow(severity_counts))) {
      cat("   -", severity_counts$value[[i]], ":", severity_counts$n[[i]], "\n")
    }
  }
  invisible(x)
}

#' @export
summary.depgraph_validation_report <- function(object, ...) {
  object$summary
}

#' @export
as.data.frame.depgraph_validation_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$issues
}

#' @export
print.split_spec <- function(x, ...) {
  cat("<split_spec>", x$constraint_mode %||% "<unknown>", "\n")
  cat("  Samples:", nrow(x$sample_data), "\n")
  cat("  Groups:", length(unique(x$sample_data[[x$group_var]])), "\n")
  if (!is.null(x$recommended_resampling)) {
    cat("  Recommended resampling:", x$recommended_resampling, "\n")
  }
  invisible(x)
}

#' @export
summary.split_spec <- function(object, ...) {
  list(
    constraint_mode = object$constraint_mode,
    constraint_strategy = object$constraint_strategy,
    n_samples = nrow(object$sample_data),
    n_groups = length(unique(object$sample_data[[object$group_var]])),
    block_vars = object$block_vars,
    time_var = object$time_var,
    ordering_required = object$ordering_required,
    recommended_resampling = object$recommended_resampling
  )
}

#' @export
as.data.frame.split_spec <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$sample_data
}

#' @export
print.split_spec_validation <- function(x, ...) {
  cat("<split_spec_validation>\n")
  cat("  Valid:", x$valid, "\n")
  cat("  Issues:", nrow(x$issues), "\n")
  invisible(x)
}

#' @export
summary.split_spec_validation <- function(object, ...) {
  object$summary
}

#' @export
as.data.frame.split_spec_validation <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$issues
}

#' @export
print.leakage_risk_summary <- function(x, ...) {
  cat("<leakage_risk_summary>\n")
  cat("  Overview:", paste(x$overview, collapse = " "), "\n")
  cat("  Diagnostics:", nrow(x$diagnostics), "\n")
  invisible(x)
}

#' @export
summary.leakage_risk_summary <- function(object, ...) {
  list(
    overview = object$overview,
    n_diagnostics = nrow(object$diagnostics),
    by_source = .depgraph_count_table(if (nrow(object$diagnostics) == 0L) character() else object$diagnostics$source),
    by_severity = .depgraph_count_table(if (nrow(object$diagnostics) == 0L) character() else object$diagnostics$severity)
  )
}

#' @export
as.data.frame.leakage_risk_summary <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$diagnostics
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
    return(y)
  }
  x
}

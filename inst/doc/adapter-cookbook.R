## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = TRUE
)

package_root <- if (file.exists("../DESCRIPTION")) ".." else "."
if (requireNamespace("pkgload", quietly = TRUE) &&
    file.exists(file.path(package_root, "DESCRIPTION"))) {
  pkgload::load_all(package_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(splitGraph)
}


## ----build-spec---------------------------------------------------------------
meta <- data.frame(
  sample_id    = c("S1", "S2", "S3", "S4", "S5", "S6"),
  subject_id   = c("P1", "P1", "P2", "P2", "P3", "P3"),
  batch_id     = c("B1", "B2", "B1", "B2", "B1", "B2"),
  timepoint_id = c("T0", "T1", "T0", "T1", "T0", "T1"),
  time_index   = c(0, 1, 0, 1, 0, 1),
  outcome_id   = c("ctrl", "case", "ctrl", "case", "case", "ctrl"),
  stringsAsFactors = FALSE
)

g <- graph_from_metadata(meta, graph_name = "cookbook")
subject_constraint <- derive_split_constraints(g, mode = "subject")
spec <- as_split_spec(subject_constraint, graph = g)
spec


## ----show-sample-data---------------------------------------------------------
as.data.frame(spec)[, c("sample_id", "group_id", "batch_group", "order_rank")]


## ----adapter-base-r-----------------------------------------------------------
logo_folds <- function(spec, observation_data, sample_id_col = "sample_id") {
  stopifnot(inherits(spec, "split_spec"))
  if (!sample_id_col %in% names(observation_data)) {
    stop("`observation_data` must contain a `", sample_id_col, "` column.")
  }

  joined <- merge(
    observation_data,
    spec$sample_data[, c("sample_id", spec$group_var)],
    by.x = sample_id_col, by.y = "sample_id", sort = FALSE
  )
  joined$.row <- seq_len(nrow(joined))
  groups <- split(joined$.row, joined[[spec$group_var]])

  lapply(names(groups), function(g) {
    list(
      group   = g,
      train   = unlist(groups[setdiff(names(groups), g)], use.names = FALSE),
      assess  = groups[[g]]
    )
  })
}

# Pretend we have an observation frame keyed by sample_id.
obs <- data.frame(
  sample_id = meta$sample_id,
  x = rnorm(nrow(meta)),
  y = rbinom(nrow(meta), 1, 0.5)
)

folds <- logo_folds(spec, obs)
length(folds)
folds[[1]]


## ----adapter-rsample-group, eval=FALSE----------------------------------------
# spec_to_group_vfold <- function(spec, observation_data,
#                                 v = NULL,
#                                 sample_id_col = "sample_id") {
#   stopifnot(inherits(spec, "split_spec"))
#   if (!requireNamespace("rsample", quietly = TRUE)) {
#     stop("Install rsample to use this adapter.")
#   }
# 
#   joined <- merge(
#     observation_data,
#     spec$sample_data[, c("sample_id", spec$group_var)],
#     by.x = sample_id_col, by.y = "sample_id", sort = FALSE
#   )
# 
#   n_groups <- length(unique(joined[[spec$group_var]]))
#   if (is.null(v)) v <- n_groups
# 
#   rsample::group_vfold_cv(
#     data  = joined,
#     group = !!spec$group_var,
#     v     = v
#   )
# }


## ----adapter-rsample-rolling, eval=FALSE--------------------------------------
# spec_to_rolling_origin <- function(spec, observation_data,
#                                    sample_id_col = "sample_id",
#                                    initial = NULL,
#                                    assess = 1L) {
#   stopifnot(inherits(spec, "split_spec"))
#   if (is.null(spec$time_var)) {
#     stop("This split_spec has no `time_var`; ordered evaluation is not available.")
#   }
#   if (!requireNamespace("rsample", quietly = TRUE)) {
#     stop("Install rsample to use this adapter.")
#   }
# 
#   joined <- merge(
#     observation_data,
#     spec$sample_data[, c("sample_id", spec$time_var)],
#     by.x = sample_id_col, by.y = "sample_id", sort = FALSE
#   )
#   ordered <- joined[order(joined[[spec$time_var]]), , drop = FALSE]
# 
#   if (is.null(initial)) initial <- max(1L, floor(nrow(ordered) * 0.6))
#   rsample::rolling_origin(ordered, initial = initial, assess = assess)
# }


## ----serialize, eval = requireNamespace("jsonlite", quietly = TRUE)-----------
tmp <- tempfile(fileext = ".json")
write_split_spec(spec, tmp)

# Inspect the first ~30 lines so the on-disk format is visible.
cat(readLines(tmp, n = 30), sep = "\n")

# And read it back exactly.
spec2 <- read_split_spec(tmp)
identical(spec$sample_data$group_id, spec2$sample_data$group_id)

unlink(tmp)


# Generates the data-flow figure used in paper.md (JOSS submission).
#
#   Rscript dev/paper-figure.R
#
# Writes paper-figures/paper-pipeline.png. Base graphics only, so the figure is
# reproducible from a bare R installation with no extra dependencies.

out <- file.path("paper-figures", "paper-pipeline.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

png(out, width = 2200, height = 900, res = 210)
op <- par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
on.exit({ par(op); dev.off() }, add = TRUE)

plot(NA, xlim = c(0, 100), ylim = c(0, 39), axes = FALSE, xlab = "", ylab = "")

col_in   <- "#E8EEF4"
col_core <- "#4C78A8"
col_out  <- "#54A24B"
col_side <- "#F4F4F4"
edge_col <- "#31465F"

box <- function(x, w, y, h, label, sub = NULL, fill, text_col = "black",
                cex = 0.70, sub_cex = 0.60) {
  rect(x, y, x + w, y + h, col = fill, border = edge_col, lwd = 1.4)
  if (is.null(sub)) {
    text(x + w / 2, y + h / 2, label, col = text_col, cex = cex, font = 2)
  } else {
    text(x + w / 2, y + h * 0.66, label, col = text_col, cex = cex, font = 2)
    text(x + w / 2, y + h * 0.30, sub, col = text_col, cex = sub_cex)
  }
}

# Pipeline geometry: five boxes of width `bw` separated by gaps of width `gw`,
# wide enough for a two-line function label to sit above each arrow.
bw <- 12.6
gw <- 8.5
x0 <- 1.5
xs <- x0 + (0:4) * (bw + gw)
ytop <- 23
ht   <- 12
ymid <- ytop + ht / 2

step <- function(i, label) {
  a <- xs[i] + bw
  b <- xs[i + 1]
  arrows(a + 0.4, ymid, b - 0.4, ymid, length = 0.065, lwd = 1.6, col = edge_col)
  text((a + b) / 2, ymid + 3.6, label[1], cex = 0.55, col = edge_col, font = 3)
  text((a + b) / 2, ymid + 1.9, label[2], cex = 0.55, col = edge_col, font = 3)
}

box(xs[1], bw, ytop, ht, "metadata table", "one row per sample", col_in)
box(xs[2], bw, ytop, ht, "dependency_graph", "11 node types, 17 relations",
    col_core, text_col = "white", sub_cex = 0.55)
box(xs[3], bw, ytop, ht, "split_constraint", "one group per sample", col_out)
box(xs[4], bw, ytop, ht, "split_spec", "declared roles + rows", col_out)
box(xs[5], bw, ytop, ht, "JSON", "schema-versioned", col_out)

step(1, c("graph_from_", "metadata()"))
step(2, c("derive_split_", "constraints()"))
step(3, c("as_split_", "spec()"))
step(4, c("write_split_", "spec()"))

# Validation hangs off the graph, not off the pipeline.
vx <- xs[2] + bw / 2
arrows(vx, ytop - 0.4, vx, 14.4, length = 0.065, lwd = 1.6, col = edge_col)
text(vx + 0.8, 18.2, "validate_graph()", cex = 0.55, col = edge_col,
     font = 3, pos = 4)
box(xs[2] - 3.6, bw + 7.2, 2.4, 12, "validation report",
    "structural / semantic / leakage", col_side, sub_cex = 0.55)

# Everything past the JSON artifact belongs to a consumer, not to splitGraph.
cx <- xs[5] + bw / 2
arrows(cx, ytop - 0.4, cx, 14.4, length = 0.065, lwd = 1.6, col = edge_col)
segments(77.5, 18.2, 98.5, 18.2, lty = 3, lwd = 1.6, col = "#B03A2E")
text(88.0, 19.9, "splitGraph stops here", cex = 0.56,
     col = "#B03A2E", font = 3)
box(77.5, 21.0, 2.4, 12, "consumers",
    "bioLeak (R)\nsplitspec + scikit-learn (Python)", fill = "#FFFFFF",
    sub_cex = 0.55)

par(op)
message("wrote ", out)

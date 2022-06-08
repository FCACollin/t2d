
#' miRNA Names
#'
#' Micro-RNA sample names were such as
#' `X222v5_smallRNA_BESD_S88_L008_R1_001.hairpin.stats`,
#' the function uses regex to match the sample name.
#'
#' @note **study specific function**.
#' @param x (`matrix`)\cr the miRNA matrix
#' @export
#'
format_mirna <- function(x) {
  rownames(x) <- x[, 1]
  x <- x[-1]
  names(x) <- gsub(pattern = "^(X.*v.).*$", x = names(x), replacement = "\\1")
  x
}

#' Count-Per-Million
#'
#' Transform counts in CPM.
#'
#' @param x (`numeric` or `matrix`).
#' @param ... arguments passed to methods, not used.
#' @export
#' @name cpm
#'
cpm <- function(x, ...) {
  UseMethod("cpm", x)
}

#' @describeIn cpm cpm for numeric.
#' @export
cpm.numeric <- function(x, ...) x / sum(x) * 1e6

#' @describeIn cpm cpm for matrix.
#' @param margin (`integer`)\cr same as `MARGIN` in [apply()].
#' @export
cpm.matrix <- function(x, margin = 2, ...) {
  apply(x, MARGIN = margin, cpm)
}

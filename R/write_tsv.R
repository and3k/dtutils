#' TSV writer
#'
#' As fwrite, but defaults to TSV instead of CSV.
#'
#' @export
#' @aliases write_tsv.data.table
#' @param x An R object.
#' @param file Output file name.
#' @param ... Additional arguments to be passed to methods.
#' @examples
#' library(data.table)
#' Mtcars <- as.data.table(mtcars, keep.rownames = TRUE)
#' write_tsv(Mtcars, "mtcars.tsv")
write_tsv <- function(x, file, ...) {
  UseMethod('write_tsv')
}

#' @export
write_tsv.data.table <- function(x, file, sep = "\t", ...) {
  data.table::fwrite(x = x, file = file, sep = sep, ...)
}

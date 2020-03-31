#' TSV writer
#'
#' As fwrite, but defaults to TSV instead of CSV.
#'
#' @export
#' @aliases write_tsv.data.table
#' @param x An R object.
#' @param file Output file name. File extensions other than `.tsv`, `.tab`, and `.txt` result in a warning.
#' @param ... Additional arguments to be passed to methods.
#' @examples
#' library(data.table)
#' Mtcars <- as.data.table(mtcars, keep.rownames = TRUE)
#' write_tsv(Mtcars, "mtcars.tsv")
write_tsv <- function(x, file, ...) {
  if(!all(stringr::str_detect(file, stringr::regex('\\.(tsv|tab|txt)$', ignore_case = TRUE)))) {
    warning('TSV files should have the extension \u2018.tsv\u2019.')
  }
  UseMethod('write_tsv')
}

#' @export
write_tsv.data.table <- function(x, file, sep = '\t', ...) {
  if(!identical(sep, '\t')) {
    stop('The separator in TSV files needs to be \u2018\\t\u2019.')
  }
  data.table::fwrite(x = x, file = file, sep = sep, ...)
}

#' TSV writer
#'
#' As [`fwrite`], but tab-separated (TSV) instead of the default comma-separated (CSV).
#'
#' @export
#' @aliases fwrite_tsv
#' @aliases write_tsv.data.table
#' @param x An \R object (e.g., a [`data.table`]).
#' @param file Output file name. File extensions other than `.tsv`, `.tab`, and `.txt` result in a warning.
#' @param ... Additional arguments to be passed to methods (e.g., to [`fwrite`]).
#' @examples
#' library(data.table)
#' library(dtutils)
#' mtcars_dt <- as.data.table(mtcars, keep.rownames = TRUE)
#' write_tsv(mtcars_dt, "mtcars.tsv")
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

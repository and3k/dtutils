#' TSV writer
#'
#' Writes an \R object to a tab-separated values (TSV) file.
#'
#' @param object An \R object to save as TSV.
#' @param file Output file name.
#'   File extensions other than `.tsv`, `.tab`, and `.txt` result in a warning.
#' @param sep Defaults to `"\t"` and cannot be changed.
#'   Included for comptability with base methods and underlying methods.
#' @param ... Additional arguments to be passed to methods.
#' @examples
#' library(data.table)
#' library(dtutils)
#' mtcars_dt <- as.data.table(mtcars, keep.rownames = TRUE)
#' write_tsv(mtcars_dt, "mtcars.tsv")
#' @export
write_tsv <- function(object, file, sep = "\t", ...) {
  if (!all(stringr::str_detect(
    file,
    stringr::regex("\\.(tsv|tab|txt)$", ignore_case = TRUE)
  ))) {
    warning("TSV files should have the extension \u2018.tsv\u2019.")
  }
  UseMethod("write_tsv")
}

#' @describeIn write_tsv Writes a [data.table] using [data.table::fwrite()].
#' @aliases fwrite_tsv
#' @export
write_tsv.data.table <- function(object, file, sep = "\t", ...) { # nolint
  if (!identical(sep, "\t")) {
    stop("The separator in TSV files needs to be \u2018\\t\u2019.")
  }
  data.table::fwrite(x = object, file = file, sep = sep, ...)
}

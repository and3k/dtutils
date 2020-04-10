#' TSV writer
#'
#' Writes an \R object to a tab-separated values (TSV) file.
#'
#' @param object An \R object to save as TSV.
#' @param file Output file name.
#'   File extensions other than `.tsv`, `.tab`, and `.txt` result in a warning.
#' @param ... Additional arguments to be passed to methods.
#' @examples
#' library(dtutils)
#' @export
write_tsv <- function(object, file, ...) {
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
#' @examples
#'
#' # write a data.table
#' mtcars_dt <- data.table::as.data.table(mtcars, keep.rownames = TRUE)
#' write_tsv(mtcars_dt, "mtcars.tsv")
#' @export
write_tsv.data.table <- function(object, file, ...) { # nolint
  data.table::fwrite(x = object, file = file, sep = "\t", ...)
}

#' @describeIn write_tsv Writes a [data.frame] using [data.table::fwrite()].
#' @param row_names For \R objects with row names.
#'   `FALSE` omits writing row names. `TRUE` writes row names.
#'   A single string can be given instead of `TRUE`
#'   to set the column name for the row names.
#'   Per default, row names are written, expect if
#'   they are automatic row names (i.e., numbered).
#' @examples
#'
#' # write a data.frame
#' write_tsv(mtcars, "mtcars.tsv")
#' write_tsv(mtcars, "mtcars.tsv", row_names = "ROWNAMES")
#' @export
write_tsv.data.frame <- function(object, file, ..., row_names) { # nolint
  if (missing(row_names)) {
    if (identical(attr(object, "row.names"), seq_len(nrow(object)))) {
      # automatic row names are not written, see also ?row.names
      row_names <- FALSE
    } else {
      row_names <- TRUE
    }
  }

  object <- data.table::as.data.table(object, keep.rownames = row_names)
  write_tsv(object, file, ...)
}

#' @describeIn write_tsv Writes a [matrix] using [data.table::fwrite()].
#' @examples
#'
#' # write a matrix
#' x <- matrix(runif(100), ncol = 5)
#' colnames(x) <- paste0("col_", letters[1:5])
#' write_tsv(x, "matrix.tsv")
#' rownames(x) <- paste0("row_", letters[1:20])
#' write_tsv(x, "matrix_with_row_names.tsv")
#' @export
write_tsv.matrix <- function(object, file, ..., row_names) { # nolint
  object <- as.data.frame(object)
  write_tsv(object, file, ...)
}

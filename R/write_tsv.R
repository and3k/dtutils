#' Internal wrapper for `fwrite`
#'
#' Calls [data.table::fwrite()] with appropriate arguments:
#' * The column separator is always `"\t"`.
#' * The line separator is always `"\n"`,
#'   independent of the operating system.
#'
#' @param object An \R object to save as TSV.
#' @param file Output file name.
#'   File extensions other than `.tsv`, `.tab`, and `.txt` result in a warning.
#' @param header Should the column names be written?
internal_write_tsv <- function(object, file, header = TRUE) {
  if (!all(stringr::str_detect(
    file,
    stringr::regex("\\.(tsv|tab|txt)$", ignore_case = TRUE)
  ))) {
    rlang::warn("TSV files should have the extension \u2018.tsv\u2019.")
  }

  data.table::fwrite(
    x = object,
    file = file,
    sep = "\t",
    eol = "\n",
    col.names = header
  )
}

#' TSV writer
#'
#' Writes an \R object to a tab-separated values (TSV) file.
#'
#' @inheritParams internal_write_tsv
#' @param ... Additional arguments to be passed to methods (see below).
#'   All additional arguments in methods are ignored.
#' @examples
#' library(dtutils)
#' @export
write_tsv <- function(object, file, ...) {
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
  internal_write_tsv(object = object, file = file)
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
write_tsv.data.frame <- function(object, file, row_names, ...) { # nolint
  if (rlang::is_missing(row_names)) {
    # seq_len(nrow(x)) is the default, see ?row.names
    row_names <- !identical(
      attr(object, "row.names"),
      seq_len(nrow(object))
    )
  }

  object <- data.table::as.data.table(object, keep.rownames = row_names)
  internal_write_tsv(object = object, file = file)
}

#' @describeIn write_tsv Writes a [matrix] using [data.table::fwrite()].
#' @param col_names Analogous to `row_names`.
#' @examples
#'
#' # write a matrix
#' x <- matrix(runif(100), ncol = 5)
#' write_tsv(x, "matrix.tsv")
#' colnames(x) <- paste0("col_", letters[1:5])
#' write_tsv(x, "matrix_with_column_names.tsv")
#' rownames(x) <- paste0("row_", letters[1:20])
#' write_tsv(x, "matrix_with_column_and_row_names.tsv")
#' @export
write_tsv.matrix <- function(object, file, row_names, col_names, ...) { # nolint
  if (rlang::is_missing(row_names)) {
    row_names <- !rlang::is_null(dimnames(object)[[1]])
  }
  if (rlang::is_missing(col_names)) {
    col_names <- !rlang::is_null(dimnames(object)[[2]])
  }

  object <-
    data.table::as.data.table(
      as.data.frame(object),
      keep.rownames = row_names
    )
  internal_write_tsv(object = object, file = file, header = col_names)
}

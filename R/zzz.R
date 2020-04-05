.onAttach <- function(libname, pkgname) { # nolint
  if (interactive() || identical(Sys.getenv("TESTTHAT"), "true")) {
    if (!identical(getOption("datatable.na.strings"), "")) {
      packageStartupMessage(
        "To get empty string/NA compatibility between ",
        "\u2018write_tsv\u2019 and \u2018fread\u2019 set:\n",
        "  options(datatable.na.strings = \"\")."
      )
    }
  }
}

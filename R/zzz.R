.onAttach <- function(libname, pkgname) {
  if(interactive()) {
    if(!identical(getOption('datatable.na.strings'), '')) {
      packageStartupMessage('To get empty string/NA compatibility between \u2018write_tsv\u2018 and \u2018fread\u2018 set:\n  options(datatable.na.strings = "").')
    }
  }
}

test_that("write_tsv() works with data.table", {
  tf <- paste0(tempfile(), ".tsv")
  dt <- data.table(
    Number = c(1, NA, 2),
    Bool = c(T, F, NA),
    String = c("foo", "bar, baz", "Lorem ipsum dolor sit amet")
  )
  write_tsv(dt, tf)
  expect_equal(fread(tf, sep = "\t"), dt)
})

test_that("write_tsv() works with special cases", {
  tf <- paste0(tempfile(), ".tsv")
  dt <- data.table(
    Number = 1:7,
    String = c(
      "foo", "bar, baz",
      "Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit",
      "", NA,
      "single quote 'hello world' test",
      "double quote \"hello world\" test"
    )
  )
  write_tsv(dt, tf)

  # KNOWN ISSUE: fread() with quoted double-quotes does not work
  # see https://github.com/Rdatatable/data.table/issues/1109
  # that is why the last row is not compared
  expect_equal(
    head(fread(tf, sep = "\t", na.strings = ""), -1),
    head(dt, -1)
  )

  res <- readLines(tf)

  i <- 1
  # no quotations around normal strings
  expect_equal(
    res[i],
    paste0(colnames(dt)[1], "\t", colnames(dt)[2])
  )
  i <- i + 1
  expect_equal(
    res[i],
    paste0(dt$Number[1], "\t", dt$String[1])
  )
  i <- i + 1
  expect_equal(
    res[i],
    paste0(dt$Number[2], "\t", dt$String[2])
  )
  i <- i + 1
  expect_equal(
    paste0(res[i], "\n", res[i + 1]),
    paste0(dt$Number[3], "\t", "\"", dt$String[3], "\"")
  )
  i <- i + 2
  # empty strings are exported as empty quoted strings
  expect_equal(
    res[i],
    "4\t\"\""
  )
  i <- i + 1
  # NAs exported as empty
  expect_equal(
    res[i],
    "5\t"
  )
  i <- i + 1
  expect_equal(
    res[i],
    paste0(dt$Number[6], "\t", dt$String[6])
  )
  i <- i + 1
  expect_equal(
    res[i],
    paste0(dt$Number[7], "\t", "\"", gsub("\"", "\"\"", dt$String[7]), "\"")
  )
})

test_that("write_tsv() warns about wrong extensions", {
  tf_tsv <- paste0(tempfile(), ".tsv")
  tf_tab <- paste0(tempfile(), ".tab")
  tf_txt <- paste0(tempfile(), ".txt")
  tf_csv <- paste0(tempfile(), ".csv") # wrong!
  dt <- data.table(x = 1:9)

  expect_warning(write_tsv(dt, tf_tsv), NA)
  expect_warning(write_tsv(dt, tf_tab), NA)
  expect_warning(write_tsv(dt, tf_txt), NA)
  expect_warning(write_tsv(dt, tf_csv), "extension ‘.tsv’")
  expect_error(expect_warning(
    write_tsv(dt, c(tf_tsv, tf_txt)),
    NA
  ))
  expect_error(expect_warning(
    write_tsv(dt, c(tf_tsv, tf_csv)),
    "extension ‘.tsv’"
  ))
  expect_error(expect_warning(
    write_tsv(dt, c(tf_csv, tf_tsv)),
    "extension ‘.tsv’"
  ))
})

test_that("write_tsv() works with data.frame and row_names = FALSE", {
  tf <- paste0(tempfile(), ".tsv")
  write_tsv(mtcars, tf, row_names = FALSE)
  df <- read.delim(tf)
  rownames(mtcars) <- NULL
  expect_equal(df, mtcars)
})

test_that("write_tsv() works with data.frame and row_names = TRUE", {
  tf <- paste0(tempfile(), ".tsv")
  write_tsv(mtcars, tf, row_names = TRUE)
  df <- read.delim(tf, row.names = 1)
  expect_equal(df, mtcars)
})

test_that("write_tsv() and non-boolean row_names values", {
  tf <- paste0(tempfile(), ".tsv")
  df <- data.frame(
    number = 1:3,
    string = c("foo", "bar, baz", "Lorem ipsum dolor sit amet"),
    stringsAsFactors = FALSE
  )
  dt <- as.data.table(df)

  # automatic row names are not written
  write_tsv(df, tf)
  expect_equal(fread(tf, sep = "\t"), dt)

  rn <- letters[1:3]
  row.names(df) <- rn

  # explicit row names are written
  write_tsv(df, tf)
  expect_equal(
    fread(tf, sep = "\t"),
    data.table(rn = rn, dt)
  )

  # row names name can be given
  write_tsv(df, tf, row_names = "these_are_the_row_names")
  expect_equal(
    fread(tf, sep = "\t"),
    data.table(these_are_the_row_names = rn, dt)
  )
})

test_that("write_tsv() works with matrix", {
  tf <- paste0(tempfile(), ".tsv")
  m <- matrix(1:50, ncol = 5)

  write_tsv(m, tf)
  expect_equal(fread(tf, sep = "\t"), as.data.table(m, keep.rownames = FALSE))

  colnames(m) <- paste0("col_", letters[1:5])
  write_tsv(m, tf)
  expect_equal(fread(tf, sep = "\t"), as.data.table(m, keep.rownames = FALSE))

  rownames(m) <- paste0("row_", letters[1:10])
  write_tsv(m, tf)
  expect_equal(fread(tf, sep = "\t"), as.data.table(m, keep.rownames = TRUE))
})

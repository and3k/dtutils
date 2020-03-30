dt1 <- data.table::data.table(
  String = c('foo', 'bar, baz', 'Lorem ipsum dolor sit amet', '', NA),
  Number = c(1, NA, 2, 3, 4),
  Bool = c(T, F, T, F, NA)
)

test_that('write_tsv() works with data.table', {
  tf <- tempfile()
  write_tsv(dt1, tf)

  expect_equal(dt1, data.table::fread(tf, na.strings = ''))

  df <- read.table(tf, header = TRUE, sep = '\t', quote = '', stringsAsFactors = FALSE)
  expect_identical(df[1, 'String'], 'foo') # no quotations around normal strings
  expect_identical(df[2, 'String'], 'bar, baz')
  expect_identical(df[3, 'String'], 'Lorem ipsum dolor sit amet')
  expect_identical(df[4, 'String'], '""') # empty strings are exported as empty quoted strings
  expect_identical(df[5, 'String'], '') # NAs exported as empty
})

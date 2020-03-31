test_that('write_tsv() works with data.table', {
  tf <- paste0(tempfile(), '.tsv')
  dt <- data.table::data.table(
    Number = c(1, NA, 2),
    Bool   = c(T, F, NA),
    String = c('foo', 'bar, baz', 'Lorem ipsum dolor sit amet')
  )
  write_tsv(dt, tf)
  expect_equal(data.table::fread(tf), dt)
})

test_that('write_tsv() works with data.table and special cases', {
  tf <- paste0(tempfile(), '.tsv')
  dt <- data.table::data.table(
    Number = 1:7,
    String = c('foo', 'bar, baz', 'Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit', '', NA, 'single quote \'hello world\' test', 'double quote "hello world" test')
  )
  write_tsv(dt, tf)

  # fread and quoted double quotes does not work, see https://github.com/Rdatatable/data.table/issues/1109
  expect_equal(
    head(data.table::fread(tf, na.strings = ''), -1),
    head(dt, -1)
  )

  dtraw <- data.table::fread(tf, sep = '')
  expect_equal( # no quotations around normal strings
    dtraw[1,][[1]],
    paste0(dt$Number[1], '\t', dt$String[1])
  )
  expect_equal(
    dtraw[2,][[1]],
    paste0(dt$Number[2], '\t', dt$String[2])
  )
  expect_equal(
    paste0(dtraw[3,][[1]], '\n', dtraw[3+1,][[1]]),
    paste0(dt$Number[3], '\t', '"', dt$String[3], '"')
  )
  expect_equal( # empty strings are exported as empty quoted strings
    dtraw[4+1,][[1]],
    '4\t""'
  )
  expect_equal( # NAs exported as empty
    dtraw[5+1,][[1]],
    '5\t'
  )
  expect_equal(
    dtraw[6+1,][[1]],
    paste0(dt$Number[6], '\t', dt$String[6])
  )
  expect_equal(
    dtraw[7+1,][[1]],
    paste0(dt$Number[7], '\t', '"', gsub('"', '""', dt$String[7]), '"')
  )
})

test_that('write_tsv() warns about wrong extensions', {
  tf_tsv <- paste0(tempfile(), '.tsv')
  tf_tab <- paste0(tempfile(), '.tab')
  tf_txt <- paste0(tempfile(), '.txt')
  tf_csv <- paste0(tempfile(), '.csv') # wrong!
  dt <- data.table::data.table(x = 1:9)

  expect_warning(write_tsv(dt, tf_tsv), NA)
  expect_warning(write_tsv(dt, tf_tab), NA)
  expect_warning(write_tsv(dt, tf_txt), NA)
  expect_warning(write_tsv(dt, tf_csv), 'extension ‘.tsv’')
  expect_error(expect_warning(write_tsv(dt, c(tf_tsv, tf_txt)), NA))
  expect_error(expect_warning(write_tsv(dt, c(tf_tsv, tf_csv)), 'extension ‘.tsv’'))
  expect_error(expect_warning(write_tsv(dt, c(tf_csv, tf_tsv)), 'extension ‘.tsv’'))
})

test_that('write_tsv() works only with sep=\\t', {
  tf <- paste0(tempfile(), '.tsv')
  dt <- data.table::data.table(x = 1:9)
  expect_error(write_tsv(dt, tf, sep = '\t'), NA)
  expect_error(write_tsv(dt, tf, sep = ','), 'separator')
})

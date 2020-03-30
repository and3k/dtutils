dt1 <- data.table::data.table(
  String = c('foo', 'bar', 'Lorem ipsum dolor sit amet'),
  Number = 1:3
)

test_that('write_tsv() works', {
  tf <- tempfile()
  write_tsv(dt1, tf)
  expect_equal(dt1, data.table::fread(tf))
})

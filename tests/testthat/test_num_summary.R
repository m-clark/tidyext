context('test num_summary')

df1 <- dplyr::tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels = c('a', 'b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace = TRUE),
  d = sample(c(TRUE, FALSE), 50, replace = TRUE)
)

test_that('num_summary returns a data frame', {
  expect_s3_class(num_summary(df1$a), 'data.frame')
})

test_that('num_summary works with a factor', {
  expect_s3_class(num_summary(df1$g1), 'data.frame')
})

test_that('num_summary works with a character string of numeric values', {
  expect_s3_class(num_summary(c('1','2','3')), 'data.frame')
})

test_that('num_summary works with a logical', {
  expect_s3_class(num_summary(df1$d), 'data.frame')
})


test_that('num_summary works with missing values', {
  expect_s3_class(num_summary(c(df1$d, NA)), 'data.frame')
})

test_that('num_summary will fail with non-numeric', {
  expect_error(num_summary(df1$c))
})

test_that('check extra', {
  expect_gt(ncol(num_summary(c(rpois(50, 2), NA, NA), extra=T)),
            ncol(num_summary(c(rpois(50, 2), NA, NA), extra=F)))
})

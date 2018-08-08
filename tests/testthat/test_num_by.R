context('test num_by')

df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c_ = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE),
  a_b = a*b,
  b_sq = b^2
)

test_that('num_by returns a data frame', {
  expect_s3_class(num_by(df1, main_var = a), 'data.frame')
})

test_that('num_by errors without data', {
  expect_error(num_by(data=NULL, main_var = a))
})

test_that('num_by errors without data', {
  expect_error(num_by(data=filter(df1, g1=='c'), main_var = a))
})

test_that('num_by takes multple main_var', {
  expect_s3_class(num_by(df1, main_var = vars(a,b)), 'data.frame')
})

test_that('num_by takes a group var', {
  expect_s3_class(num_by(df1, main_var = a, group_var = g2), 'data.frame')
})

test_that('num_by will take digits', {
  expect_s3_class(num_by(df1, main_var = a, group_var = g2, digits=2),
                  'data.frame')
})

test_that('num_by fails on non-numeric', {
  expect_error(num_by(df1, main_var = c_))
})

test_that('num_by fails with non-data.frame object', {
  expect_error(num_by(as.matrix(df1[,'a']), main_var = a))
})

test_that('num_by is ok with logical', {
  expect_s3_class(num_by(df1, main_var = d, group_var = g2), 'data.frame')
})

test_that('num_by can handle underscores in variable names', {
  res <- num_by(df1, main_var = vars(a_b, b_sq), group_var = g2)
  expect_equivalent(unique(res$Variable), c('a_b', 'b_sq'))
})


test_that('num_by can use helper functions', {
  # don't use matches because testthat::matches will screw it up
  res <- num_by(df1, main_var = vars(one_of('a','b')), group_var = g2)
  expect_equivalent(unique(res$Variable), c('a', 'b'))
  res <- num_by(df1, main_var = vars(starts_with('a')), group_var = g2)
  expect_s3_class(res, 'data.frame')
  res <- num_by(df1, main_var = vars(ends_with('a')), group_var = g2)
  expect_s3_class(res, 'data.frame')
  res <- num_by(df1, main_var = vars(contains('a')), group_var = g2)
  expect_s3_class(res, 'data.frame')
})

test_that('num_by wont fail with already grouped data', {
  expect_s3_class(num_by(group_by(df1, g1), main_var = d, group_var = g2), 'data.frame')
})


test_that('num_by can handle select not', {
  df2 = select(df1, g1, a, b, d, a_b)
  expect_s3_class(num_by(df2, main_var = -b, group_var = g1), 'data.frame')
})

test_that('num_by can handle select not', {
  df2 = select(df1, a, b, d, a_b)
  expect_s3_class(num_by(df2, main_var = -b), 'data.frame')
})


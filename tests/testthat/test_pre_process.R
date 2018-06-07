
context('test pre_process')

set.seed(1234)
df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE),
  a_b = a*b,
  b_sq = b^2
)

test_that('pre_process returns a dataframe', {
  expect_s3_class(pre_process(df1), 'data.frame')
})

test_that('pre_process can do nothing', {
  expect_identical(pre_process(df1, std=F), df1)
})

test_that('pre_process will fail if scale_by is not numeric', {
  expect_error(pre_process(df1, scale_by = F))
})

test_that('pre_process can center', {
  init <- pre_process(df1, scale_by = 0)
  expect_equal(round(mean(init$b), 2), 0)
})

test_that('pre_process can standardize with specific input', {
  init <- pre_process(df1, scale_by = 2)
  expect_equal(round(sd(init$b), 2), .5)
})

test_that('pre_process can log', {
  init <- pre_process(mtcars, log_vars=vars(mpg, wt))
  expect_equal(exp(init$mpg), mtcars$mpg)
})

test_that('pre_process can log with specific base', {
  expect_is(pre_process(mtcars, log_vars=vars(mpg, wt), log_base = log(2)),
            'data.frame')
})

test_that('pre_process can zero_start', {
  init <- pre_process(df1, zero_start = vars(b))
  expect_equal(min(init$b), 0)
})

test_that('pre_process returns a zero one', {
  init <- pre_process(df1, zero_one = vars(b))
  expect_equal(max(init$b), 1)
})

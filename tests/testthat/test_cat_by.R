context('test cat_by')

set.seed(1)
df1 <- dplyr::tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 1),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE)
)


test_that('cat_by returns a data frame', {
  expect_s3_class(cat_by(df1, main_var = g1), 'data.frame')
})

test_that('cat_by errors without data', {
  expect_error(cat_by(data=NULL, main_var = a))
})

test_that('cat_by errors without data', {
  expect_error(cat_by(data=filter(df1, g1=='c'), main_var = a))
})


test_that('cat_by takes multiple main vars', {
  expect_s3_class(cat_by(df1, main_var = vars(g1,d)), 'data.frame')
})

test_that('cat_by takes a group var', {
  expect_s3_class(cat_by(df1, main_var = g1, group_var = g2), 'data.frame')
})

test_that('cat_by takes multiple main_vars and a group var', {
  expect_s3_class(cat_by(df1, main_var = vars(g1,d), group_var = g2),
                  'data.frame')
})

test_that('cat_by will take digits', {
  expect_s3_class(cat_by(df1, main_var = g1, group_var = g2, digits=2),
                  'data.frame')
})


test_that('cat_by will do percentages out of total', {
  cb1 <- cat_by(df1, main_var = g1, group_var = g2, digits=2)
  cb2 <- cat_by(df1, main_var = g1, group_var = g2, perc_by_group=F, digits=2)
  expect_false(identical(cb1, cb2))
})


test_that('cat_by takes ordered/multi-class without issue', {
  df1$g2 <- as.ordered(df1$g2)
  expect_s3_class(cat_by(df1, main_var = g2), 'data.frame')
})

test_that('cat_by warns on numeric', {
  expect_warning(cat_by(df1, main_var = g2))
})

test_that('cat_by warns on too many levels', {
  expect_warning(cat_by(df1, main_var = c))
})

test_that('cat_by wont fail with already grouped data', {
  expect_s3_class(cat_by(group_by(df1, g1), main_var = d, group_var = g2), 'data.frame')
})

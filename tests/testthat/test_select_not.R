context('test select_not')

test_that('select_not works', {
  expect_equal(ncol(tidyext:::select_not(mtcars, mpg, wt)), ncol(mtcars)-2)
})

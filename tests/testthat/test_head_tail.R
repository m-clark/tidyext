
context('test head_tail')


test_that('head_tail returns a data.frame', {
  expect_s3_class(head_tail(iris), 'data.frame')
})


test_that('head_tail will work on matrices', {
  expect_s3_class(head_tail(as.matrix(mtcars)), 'data.frame')
})


test_that('head_tail will not be limited to tibble printing', {
  expect_equal(nrow(head_tail(dplyr::as_tibble(iris), 16)), 32)
})


test_that('head_tail will fail if not a matrix/data.frame', {
  expect_error(head_tail(1:3))
})


context('test rnd')


test_that('rnd returns a character vector', {
  expect_type(rnd(rnorm(10)), 'character')
})

test_that('rnd returns correct decimal places even if an integer', {
  expect_equal(nchar(rnd(1)), 5)
})

test_that('rnd will fail if not numeric', {
  expect_error(rnd('a'))
})

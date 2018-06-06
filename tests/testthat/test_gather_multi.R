context('test gather_multi')

# debugonce(gather_multi)
# test = gather_multi(demo_data_wide,
#              key = wave,
#              values = c('X', 'Y', 'Z'),
#              varlist = c(vars(starts_with('X')),
#                             vars(starts_with('Y')),
#                             vars(starts_with('Z'))),
#              -id)
# test
demo_data_wide = data.frame(id = 1:10,
                            X = matrix(rnorm(40), ncol = 4),
                            Y = matrix(sample(0:1, 40, replace = T), ncol = 4),
                            Z = matrix(rpois(40, 5), ncol = 4))

set.seed(123)
demo_data_wide_miss = demo_data_wide
demo_data_wide_miss[sample(1:nrow(demo_data_wide), 3), 'X.1'] = NA
demo_data_wide_miss[sample(1:nrow(demo_data_wide), 3), 'Y.2'] = NA
demo_data_wide_miss[sample(1:nrow(demo_data_wide), 3), 'Z.3'] = NA

test_that('gather_multi returns a data frame', {
  test_dat = gather_multi(demo_data_wide,
               key = wave,
               values = c('X', 'Y', 'Z'),
               varlist = c(vars(starts_with('X')),
                              vars(starts_with('Y')),
                              vars(starts_with('Z'))))
  expect_is(test_dat, 'data.frame')
})

test_that('gather_multi takes dots', {
  test_dat = gather_multi(demo_data_wide,
               key = wave,
               values = c('X', 'Y', 'Z'),
               varlist = c(vars(starts_with('X')),
                              vars(starts_with('Y')),
                              vars(starts_with('Z'))),
               -id)
  expect_is(test_dat, 'data.frame')
})

test_that('gather_multi takes vars with values', {
  test_dat = gather_multi(demo_data_wide,
               key = wave,
               values = vars(X, Y, Z),
               varlist = c(vars(starts_with('X')),
                              vars(starts_with('Y')),
                              vars(starts_with('Z'))),
               -id)
  expect_is(test_dat, 'data.frame')
})

test_that('gather_multi warns with na.rm = TRUE', {
  expect_warning(gather_multi(demo_data_wide_miss,
                              key = wave,
                              values = c('X', 'Y', 'Z'),
                              varlist = c(vars(starts_with('X')),
                                          vars(starts_with('Y')),
                                          vars(starts_with('Z'))),
                              -id,
                              na.rm = TRUE))
})

test_that('gather_multi errors with imbalance', {
  expect_error(gather_multi(demo_data_wide_miss,
                            key = wave,
                            values = c('X', 'Z'),
                            varlist = c(vars(starts_with('X')),
                                        vars(starts_with('Y')),
                                        vars(starts_with('Z'))),
                            -id))
})

test_that('gather_multi errors with imbalance', {
  expect_error(gather_multi(demo_data_wide_miss,
                            key = wave,
                            values = c('X', 'Y', 'Z'),
                            varlist = c(vars(starts_with('X')),
                                        vars(starts_with('Y')),
                                        vars(one_of('Z.1', 'Z.2'))),
                            -id))
})

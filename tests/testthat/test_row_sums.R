context('test rowwise functions')

d = data.frame(x = 1:3,
               y = 4:6,
               z = 7:9,
               q = NA)
# d  %>%
#   row_sums(x:y)
# d  %>%
#   row_means(matches('x|z'))
# row_apply(
#   d ,
#   everything(),
#   .fun = function(x)
#     apply(x, 1, median)
# )



# row_sums ----------------------------------------------------------------

test_that('row_sums returns a data frame', {
  expect_s3_class(d %>% row_sums(x:z), 'data.frame')
})

test_that('row_sums returns correct values', {
  expect_equal(d %>% row_sums(x:z) %>% pull(sum), c(12, 15, 18))
})

test_that('row_sums handles NA', {
  expect_equal(d %>% row_sums(x:z, na_rm = TRUE) %>% pull(sum), c(12, 15, 18))
})

test_that('row_sums takes varname', {
  expect_equal(d %>% row_sums(x:z, varname = 'blah') %>% pull(blah),
               c(12, 15, 18))
})


test_that('row_sums handles select helpers', {
  expect_equal(d %>% row_sums(dplyr::matches('x|z')) %>% pull(sum),
               c(8, 10, 12))
})



# row_means ---------------------------------------------------------------


test_that('row_means returns a data frame', {
  expect_s3_class(d %>% row_means(x:z), 'data.frame')
})

test_that('row_means returns correct values', {
  expect_equal(d %>% row_means(x:z) %>% pull(mean), 4:6)
})

test_that('row_means handles NA', {
  expect_equal(d %>% row_means(x:q, na_rm = TRUE) %>% pull(mean), 4:6)
})

test_that('row_means takes varname', {
  expect_equal(d %>% row_means(x:z, varname = 'blah') %>% pull(blah), 4:6)
})


test_that('row_means handles select helpers', {
  expect_equal(d %>% row_means(dplyr::matches('x|z')) %>% pull(mean), 4:6)
})


# row_min_max -------------------------------------------------------------

test_that('row_max returns a data frame', {
  expect_s3_class(d %>% row_max(x:z), 'data.frame')
  expect_s3_class(d %>% row_min(x:z), 'data.frame')
})

test_that('row_max returns correct values', {
  expect_equal(d %>% row_max(x:z) %>% pull(max), 7:9)
  expect_equal(d %>% row_min(x:z) %>% pull(min), 1:3)
})

test_that('row_max handles NA', {
  expect_equal(d %>% row_max(x:q, na_rm = TRUE) %>% pull(max), 7:9)
  expect_equal(d %>% row_min(x:q, na_rm = TRUE) %>% pull(min), 1:3)
})

test_that('row_max takes varname', {
  expect_equal(d %>% row_max(x:z, varname = 'blah') %>% pull(blah), 7:9)
  expect_equal(d %>% row_min(x:z, varname = 'blah') %>% pull(blah), 1:3)
})

test_that('row_max handles select helpers', {
  expect_equal(d %>% row_max(dplyr::matches('x|z')) %>% pull(max), 7:9)
  expect_equal(d %>% row_min(dplyr::matches('x|z')) %>% pull(min), 1:3)
})


# row_apply ---------------------------------------------------------------

fun_med = function(x) apply(x, 1, median)
fun_paste = function(x) apply(x, 1, paste0, collapse = '')

test_that('row_apply returns a data frame', {
  expect_s3_class(d %>% row_apply(x:z, .fun = fun_med), 'data.frame')
})

test_that('row_apply returns correct values', {
  expect_equal(d %>% row_apply(x:z, .fun = fun_med) %>% pull(var), 4:6)
})

test_that('row_apply returns correct values', {
  expect_equal(d %>% row_apply(x:z, .fun = fun_paste) %>% pull(var),
               c("147", "258", "369"))
})

test_that('row_means takes varname', {
  expect_equal(d %>%
                 row_apply(x:z, .fun = fun_med, varname = 'blah') %>%
                 pull(blah),
               4:6)
})


test_that('row_means handles select helpers', {
  expect_equal(d %>%
                 row_apply(dplyr::matches('x|z'), .fun = fun_med) %>%
                 pull(var),
               4:6)
})

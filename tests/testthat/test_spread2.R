
context('test spread2')

set.seed(1234)
# original example from SO
df1 <- structure(list(age = c("21", "17", "32", "29", "15"),
                      gender = structure(c(2L, 1L, 1L, 2L, 2L),
                                         .Label = c("Female", "Male"),
                                         class = "factor")),
                 row.names = c(NA, -5L),
                 class = c("tbl_df", "tbl", "data.frame"),
                 .Names = c("age", "gender"))

test_that('pre_process returns a dataframe', {
  expect_s3_class(spread2(df1, key=gender, value=age), 'data.frame')
})

test_that('pre_process can do compact arg', {
  expect_s3_class(spread2(df1, key=gender, value=age, compact = F),
                  'data.frame')
})

test_that('pre_process can handle NA', {
  stocks_wide <- data.frame(
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  )

  stocks_long <- stocks_wide %>% tidyr::gather(stock, price)
  stocksm <- stocks_long
  stocksm$price[sample(seq_along(stocksm$price), 5)] <- NA

  expect_identical(nrow(spread2(stocksm, stock, price)),
                   nrow(spread2(stocks_long, stock, price)))
})

#' Spread without frustration
#'
#' @param data See \code{\link[tidyr]{spread}}.
#' @param key See \code{\link[tidyr]{spread}}.
#' @param value See \code{\link[tidyr]{spread}}.
#' @param compact When you don't have a unique identifier there is more than one
#'   way the output could be produced. Actually two. One is rather messy but may
#'   in fact be what is desired. The other is more 'compact'. See the examples.
#' @param fill See \code{\link[tidyr]{spread}}.
#' @param convert See \code{\link[tidyr]{spread}}.
#' @param drop See \code{\link[tidyr]{spread}}.
#' @param sep See \code{\link[tidyr]{spread}}.
#' @details Anyone that uses spread more than once has likely come across the
#'   duplicate identifiers problem.  This function works just like select (it's
#'   mostly just a wrapper) except that it works when you don't have unique
#'   identifiers, which is actually quite often.  The only additional
#'   functionality regards the \code{compact} argument. See the examples for
#'   what this means.
#'
#'   For more, see
#'   \href{https://stackoverflow.com/questions/45898614/how-to-spread-columns-with-duplicate-identifiers?rq=1}{StackOverflow}
#'    for example, or the numerous repeated issue brought up at the
#'   \href{https://github.com/tidyverse/tidyr/issues}{tidyr GitHub issues
#'   page}. Search for 'spread duplicate' for all, not just open issues (direct link couldn't be handled
#'   by R documentation).
#'
#' @return A data frame with 'wide' format.
#' @seealso \code{\link[tidyr]{spread}}
#' @importFrom tibble rowid_to_column
#' @examples
#' library(tidyext); library(tidyr)
#'
#' # initial example from spread
#' stocks_init <- data.frame(
#' time = as.Date('2009-01-01') + 0:9,
#' X = rnorm(10, 0, 1),
#' Y = rnorm(10, 0, 2),
#' Z = rnorm(10, 0, 4)
#' )
#'
#' # a very common situation
#' stocks <- data.frame(
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#'
#'
#' stocksm_init <- stocks_init %>% gather(stock, price, -time)
#' stocksm_init %>% spread(stock, price)    # works fine
#' stocksm <- stocks %>% gather(stock, price)
#' # stocksm %>% spread(stock, price)       # annoying
#' stocksm %>% spread2(stock, price)        # works fine
#' stocksm %>% spread2(stock, price, compact = FALSE)
#'
#' # works with NA
#' stocksm$price[sample(1:nrow(stocksm), 5)] = NA
#' stocksm %>% spread2(stock, price)
#' stocksm %>% spread2(stock, price, compact = FALSE)
#'
#' @export
spread2 <- function(data,
                    key,
                    value,
                    compact = TRUE,
                    fill = NA,
                    convert = FALSE,
                    drop = TRUE,
                    sep = NULL) {
  k <- enquo(key)
  v <- enquo(value)

  if (compact) {
    data <- data %>%
      bind_cols(data %>%
                  select(-!!v) %>%
                  group_by_all() %>%
                  mutate(rowid = row_number()) %>%
                  ungroup() %>%
                  select(rowid)
                )
  } else {
    data <- data %>%
      bind_cols(data %>%
                  tibble::rowid_to_column() %>%
                  select(rowid)
                )
  }
  data %>%
    tidyr::spread(key = !!k,
                  value = !!v,
                  fill = fill,
                  convert = convert,
                  drop = drop,
                  sep = sep) %>%
    select(-rowid)
}




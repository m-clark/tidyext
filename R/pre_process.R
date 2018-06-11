#' Perform common preprocessing of numeric data
#' @description Perform common data transformations
#'
#' @param data A data frame or tibble.
#' @param std Standardize numeric/integer variables?
#' @param scale_by A single value to standardize by. See details. Default is 1.
#' @param log_vars Which variables to log. Requires vars().
#' @param log_base Log base. Default is exp(1).
#' @param zero_start Which variables to start by zero. Requires vars().
#' @param zero_one Which variables to rescale from 0 to 1. Requires vars().
#'
#' @details  At a minimum, by default, this function will standardize
#'   numeric/integer variables in a data set by the value provided to std (1
#'   standard deviation is the default for scale_by). If scale_by is set to
#'   zero, the variables will simply be centered and not scaled.  This operation
#'   will only be performed on variables not provided for the other options.
#'
#'   - It will log variables contained within vars(), with base equal to
#'   log_base, which is exp(1) by default (i.e. natural log).
#'
#'   - zero_start will make the minimum value zero, as often done for time index
#'   variables in longitudinal data.

#'   - zero_one will rescale variables to range from 0 to 1.
#'
#'   This is a bare minimum function, meant to perform common operations
#'   quickly/easily. If you're wanting to do more than this, you'll have to do
#'   it yourself.
#'
#'   I would have called this function transmute, but it's already taken by
#'   dplyr, despite no one ever using it for the purpose of turning into a
#'   whirlwind capable of taking out whatever the Galactor may have in store for
#'   them.
#' @importFrom scales rescale
#' @return A data frame that has been processed
#' @export
#'
#' @examples
#' library(tidyext)
#' pre_process(mtcars)
#' pre_process(mtcars, log_vars=vars(mpg, wt))
#' pre_process(mtcars, zero_start=vars(cyl, gear))
#' pre_process(mtcars, zero_one=vars(mpg))
pre_process <- function(data,
                        std = TRUE,
                        scale_by = 1,
                        log_vars = NULL,
                        log_base = exp(1),
                        zero_start = NULL,
                        zero_one = NULL) {

  if (!is.numeric(scale_by)) stop('scale_by needs to be numeric.')
  check_log_var <- !is.null(log_vars)
  check_zero_start <- !is.null(zero_start)
  check_zero_one <- !is.null(zero_one)

  if (check_log_var | check_zero_start | check_zero_one) {
    if (check_log_var) {
      data <- data %>%
        mutate_at(log_vars, log, base=log_base)
    }
    if (check_zero_start) {
      data <- data %>%
        mutate_at(zero_start, function(x) x - min(x, na.rm = TRUE))
    }
    if (check_zero_one) {
      data <- data %>%
        mutate_at(zero_one, scales::rescale)
    }
  }
  if (std) {
    num_cols <- data %>%
      select_not(!!!log_vars, !!!zero_start, !!!zero_one) %>%
      select_if(function(x) inherits(x, c('numeric', 'integer'))) %>%
      colnames()
    data <- data %>%
      mutate_at(num_cols,
                function(x)
                  scale(x, scale = ifelse(scale_by,
                                          scale_by*sd(x, na.rm = TRUE),
                                          FALSE))[,1])
  }
  data
}



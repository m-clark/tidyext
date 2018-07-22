#' Extend tidyr gather to multiple sets of variables
#' @description  This function extends gather to work on multiple sets of
#'   columns.
#'
#' @param data A data frame.
#' @param key See \code{\link[tidyr]{gather}}.
#' @param values Multiple values as a character vector, or as unquoted using vars().
#'   See \code{\link[tidyr]{gather}}.
#' @param varlist A vector with elements that are created with
#'   \code{\link[dplyr]{vars}}.  See details.
#' @param ... See \code{\link[tidyr]{gather}}.
#' @param na.rm In this setting it typically makes little sense to set this to
#'   TRUE. A warning will be provided. See \code{\link[tidyr]{gather}}.
#' @param convert See \code{\link[tidyr]{gather}}.
#' @param key_func A function to apply to the key variable. See details.
#' @param factor_key See \code{\link[tidyr]{gather}}.
#'
#' @details This function is an attempt to extend the tidyr
#'   \code{\link[tidyr]{gather}} function to deal with more than one set of
#'   inputs, which is very common in longitudinal design and survey data
#'   generally. It will return the same thing as gather but with extra columns
#'   for each of the values you're wishing to construct.
#'
#'   \bold{The \code{values} and \code{varlist} arguments must be of equal
#'   length}. All of the following types of approaches will work:
#'
#'   \code{values = vars(X, Y)}
#'
#'   \code{values = c('X', 'Y')}
#'
#'   For those values, you could use the following:
#'
#'   \code{varlist = vars(starts_with('X'), starts_with('Y'))}
#'
#'   \code{varlist = vars(c(X.1, X.2), c(Y.1, Y.2))}
#'
#'   Technically, even this:
#'
#'   \code{varlist = list(c('X.1', 'X.2'), c('Y.1', 'Y.2'))}
#'
#'   But it's not recommended as you wouldn't have access to the tidyselect
#'   helper functions. It would even take integers but I'll not demonstrate poor
#'   programming practice.
#'
#'   \cr
#'   However, the following would not work with the above values, because it has
#'   four elements instead of two:
#'
#'   \code{varlist = vars(X.1, X.2, Y.1, Y.2)}
#'
#'   \cr \cr
#'   Often the key is made up of variable names we may not want to use. The
#'   \code{key_func} argument can be used for this as a shortcut to a separate
#'   mutate step.  It is up to you to create a function that does what you want.
#'
#'
#'
#' @note
#'   At present it will only keep the first 'key', as the rest are redundant.
#'   You can use the key_func argument to pretty it up.
#'
#' @return A data frame in so-called 'long' format with columns \code{values}.
#'
#' @examples
#' library(tidyext); library(dplyr)
#'
#' # example of longitudinal data with 4 waves
#' demo_data_wide = data.frame(id = 1:10,
#'                             X = matrix(rnorm(40), ncol = 4),
#'                             Y = matrix(sample(0:1, 40, replace = TRUE),
#'                             ncol = 4),
#'                             Z = matrix(rpois(40, 5), ncol = 4))
#'
#' test <- gather_multi(demo_data_wide,
#'                      key = wave,
#'                      values = vars(X, Y, Z),
#'                      varlist = vars(starts_with('X'),
#'                                     starts_with('Y'),
#'                                     starts_with('Z')))
#' test
#'
#' test <- gather_multi(demo_data_wide,
#'                      key = wave,
#'                      values = c('X', 'Y', 'Z'),
#'                      varlist = vars(starts_with('X'),
#'                                     starts_with('Y'),
#'                                     starts_with('Z')),
#'                      key_func = function(x) substr(x, start=3, stop=3))
#' test
#' @importFrom rlang :=
#' @importFrom tidyr gather
#' @export
gather_multi <- function(data,
                         key = "key",
                         values = "values",
                         varlist = list(),
                         ...,
                         key_func = NULL,
                         na.rm = FALSE,
                         convert = FALSE,
                         factor_key = FALSE) {

  # check if list of vars = length of values
  if (length(values) != length(varlist))
    stop('Number of values not equal to length of varlist.
         Data must be balanced.')

  # check if col lengths are the same
  test <- varlist %>% map_int(function(v) ncol(select(data, !!v)))
  if (!all(test == max(test)))
    stop('Number of columns to be gathered for each variable must be equal
  in value. Data must be balanced.')

  if (na.rm) warning('You set na.rm = TRUE. This is equivalent to doing
  casewise deletion.  Missing on any values in the
  resulting data set will be missing on all variables.')

  k <- enquo(key)

  # first gather
  data_long <- data %>%
    select_not(!!!varlist[-1]) %>%
    tidyr::gather(key = !!k,
                  value = !!values[[1]],
                  !!varlist[[1]],
                  ...,
                  na.rm = na.rm,
                  convert = convert,
                  factor_key = factor_key) %>%
    rowid_to_column()

  for (i in 2:length(varlist)) {
    data_long <- data %>%
      select_not(!!!varlist[-i]) %>%
      tidyr::gather(key = !!k,
                    value = !!values[[i]],
                    !!varlist[[i]],
                    ...,
                    na.rm = na.rm,
                    convert = convert,
                    factor_key = factor_key) %>%
      rowid_to_column()%>%
      select(rowid, !!values[[i]]) %>%
      left_join(data_long, ., by='rowid')
  }

  if (!is.null(key_func)) {
    keyname <- quo_name(k)
    data_long <- data_long %>%
      mutate(!!keyname := key_func(!!k))
  }

  data_long %>% select(-contains('rowid'))
}




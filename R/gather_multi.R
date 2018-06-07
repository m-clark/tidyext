#' Gather multiple sets of variables
#' @description  This function extends gather to work on multiple sets of
#'   columns.
#' @param data A data frame.
#' @param key See \code{\link[tidyr]{gather}}.
#' @param values Multiple values as a character vector or unquoted using vars().
#'   See \code{\link[tidyr]{gather}}.
#' @param varlist A list with elements that are created with
#'   \code{\link[dplyr]{vars}}.  See details.
#' @param ... See \code{\link[tidyr]{gather}}.
#' @param na.rm In this setting it typically makes little sense to set this to
#'   TRUE. A warning will be provided. See \code{\link[tidyr]{gather}}.
#' @param convert See \code{\link[tidyr]{gather}}.
#' @param factor_key See \code{\link[tidyr]{gather}}.
#' @details This function is an attempt to extend the tidyr
#'   \code{\link[tidyr]{gather}} function to deal with more than one set of
#'   inputs, which is very common in longitudinal design and survey data
#'   generally. It will return the same thing as gather but with extra columns
#'   for each of the values you're wishing to construct.
#'
#'   Notes:
#'
#'   At present it will only keep the first 'key', as the rest are redundant.  I
#'   may extend this functionality in the future to handle a provided key or
#'   possibly a function to apply to the resulting variable.
#' @return A data frame in so-called 'long' format with columns \code{values}.
#' @export
#'
#' @examples
#' library(tidyext); library(dplyr)
#' # example of longitudinal data with 4 waves
#' demo_data_wide = data.frame(id = 1:10,
#'                             X = matrix(rnorm(40), ncol = 4),
#'                             Y = matrix(sample(0:1, 40, replace = TRUE),
#'                             ncol = 4),
#'                             Z = matrix(rpois(40, 5), ncol = 4))
#'
#' test = demo_data_wide %>%
#'   gather_multi(key = wave,
#'                values = c('X', 'Y', 'Z'),
#'                varlist = c(vars(starts_with('X')),
#'                            vars(starts_with('Y')),
#'                            vars(starts_with('Z'))),
#'                -id)
#' test

gather_multi <- function(data,
                         key = "key",
                         values = "values",
                         varlist = list(),
                         ...,
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

  # first gather
  data_long <- data %>%
    select_not(!!!varlist[-1]) %>%
    gather(key = key,
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
      gather(key = key,
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

  data_long %>% select(-contains('rowid'))
}




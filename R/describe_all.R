#' @title Describe your data cleanly and effectively
#'
#' @description Describe data sets with multiple variable types effectively.
#'
#' @param data The dataset, of class data.frame.
#' @param digits See \code{\link[base]{round}}. Default is 2, which for
#'   categorical is applied to the proportion (i.e. before converting to
#'   percentage).
#' @param include_NAcat Include NA values as categorical levels? Default is
#'   \code{TRUE}.
#' @param NAcat_include Deprecated alias of \code{include_NAcat}.
#' @param max_levels The maximum number of levels you want to display for
#'   categorical variables. Default is 10.
#' @param include_numeric For categorical summary, also include numeric
#'   variables with unique values fewer or equal to \code{max_levels}? Default
#'   is \code{FALSE}.
#' @param sort_by_freq Sort categorical levels by frequency? Default is
#'   \code{TRUE}.
#' @param as_ordered Return the categorical results with the levels as ordered.
#'   See details and example.
#' @param ...  Additional arguments passed to \code{num_summary}
#'
#' @import dplyr
#' @importFrom stats median sd
#' @importFrom purrr map map_int map_df
#'
#' @details This function comes out of my frustrations from various data set
#'   summaries either being inadequate for my needs, too 'busy' with output, or
#'   unable to deal well with mixed data types.
#'
#'   Numeric data is treated separately from categorical, and provides the same
#'   information as in \code{\link[tidyext]{num_summary}}.
#'
#'   Categorical variables are defined as those with class 'character', 'factor',
#'   'logical', 'ordered', combined with \code{include_numeric}. They are are
#'   summarized with frequencies and percentages. For empty categorical
#'   variables (e.g. after a subset), a warning is thrown. Note that max_levels
#'   is used with \link[dplyr]{top_n}, and so will return additional values when
#'   there are ties.
#'
#'   The \code{as_ordered} argument is to get around the notorious alphabetical
#'   ordering of ggplot. It returns a data.frame where the 'data' column
#'   contains the frequency information of the categorical levels, while leaving
#'   the levels \emph{in order} (e.g. decreasing if \code{sort_by_freq} was
#'   \code{TRUE}). This way you can directly plot the result in the manner
#'   you've actually requested. See the example.
#'
#'   The functions \code{describe_all_num} and  \code{describe_all_cat} will
#'   provide only numeric or only categorical data summaries respectively.
#'   \code{describeAll} is a deprecated alias.
#'
#' @return A list with two elements of summaries for numeric and other variables
#'   respectively. Or the contents of those elements if the type-specific
#'   functions are used.
#'
#' @seealso \code{\link[base]{summary}} \code{\link[tidyext]{num_by}}
#'   \code{\link[tidyext]{num_summary}}
#'
#' @examples
#' library(tidyext); library(dplyr)
#' X = data.frame(f1 =gl(2, 1, 20, labels=c('A', 'B')),
#'                f2=gl(2, 2, 20, labels=c('X', 'Q')))
#' X = X %>% mutate(bin1 = rbinom(20, 1, p=.5),
#'                  logic1 = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'                  num1 = rnorm(20),
#'                  num2 = rpois(20, 5),
#'                  char1 = sample(letters, 20, replace = TRUE))
#' describe_all(X)
#'
#' describe_all(data.frame(x=factor(1:7)), digits=5)
#' describe_all(mtcars, digits=5, include_numeric=TRUE, max_levels=3)
#'
#' library(ggplot2)
#' freqs = describe_all_cat(mtcars,
#'                          include_numeric=TRUE,
#'                          max_levels=3,
#'                          as_ordered = TRUE)
#' freqs %>%
#'   filter(Variable == 'cyl') %>%
#'   tidyr::unnest() %>%
#'   ggplot(aes(x=Group, y=`%`)) +
#'   geom_point(size = 10)
#'
#' @export
describe_all <- function(data,
                         digits = 2,
                         include_NAcat = TRUE,
                         max_levels = 10,
                         include_numeric = FALSE,
                         NAcat_include = NULL,
                         sort_by_freq = TRUE,
                         ...) {

  if(!inherits(data, 'data.frame'))
    stop('Need a data.frame class object.')

  if(!inherits(data, 'grouped_df'))
    data <- dplyr::ungroup(data)

  if(!is.null(NAcat_include)) include_NAcat <- NAcat_include

  data_num <- describe_all_num(data, digits = digits, ...)

  data_cat <- describe_all_cat(data,
                               digits = digits,
                               include_NAcat = include_NAcat,
                               max_levels = max_levels,
                               include_numeric = include_numeric,
                               sort_by_freq = sort_by_freq)

  list(`Numeric Variables` = data_num, `Categorical Variables` = data_cat)
}

#' @rdname describe_all
#' @export
describe_all_num <- function(data, digits = 2, ...) {

  if(!inherits(data, 'data.frame')) stop('Need a data.frame class object.')

  data_num <- data %>%
    select_if(is.numeric)

  nc_num <- ncol(data_num)

  if (nc_num > 0) {
    cnames <- colnames(data_num)

    data_num <- data_num %>%
      purrr::map_df(num_summary, digits = digits, ...) %>%
      mutate(Variable = cnames) %>%
      select(Variable, everything())
  } else {
    data_num <- message('No numeric data.')
  }
  data_num
}


#' @rdname describe_all
#' @export
describe_all_cat <- function(data,
                             digits = 2,
                             include_NAcat = TRUE,
                             max_levels = 10,
                             include_numeric = FALSE,
                             sort_by_freq = TRUE,
                             as_ordered = FALSE
                             ) {

  if(!inherits(data, 'data.frame')) stop('Need a data.frame class object.')

  data_cat <- data %>%
    select_if(function(x)
      inherits(x, c('character', 'factor', 'logical', 'ordered')) |
      include_numeric && n_distinct(x) <= max_levels)

  nc_cat <- ncol(data_cat)

  if (nc_cat > 0) {
    data_cat <- data_cat %>%
      mutate_all(as.character)

    cat_names <- names(data_cat)

    nlevs <- data_cat %>%
      purrr::map_int(function(x) if_else(all(is.na(x)),
                                         0L,
                                         if_else(include_NAcat,
                                                 n_distinct(x),
                                                 n_distinct(na.omit(x)))))

    if (any(nlevs == 0))
      warning(paste0(names(nlevs)[nlevs==0],
                     ' have no category levels and will be dropped.\n'))

    if (any(nlevs > 0)){
      data_cat <- data_cat %>%
        select_if(nlevs > 0) %>%
        purrr::map(function(x)
          data.frame(x=table(x, useNA = if_else(include_NAcat,
                                                'ifany',
                                                'no')),
                     y=prop.table(table(x, useNA = if_else(include_NAcat,
                                                           'ifany',
                                                           'no')))) %>%
            select(-y.x) %>%
            rename(Group=x.x,
                   Frequency = x.Freq,
                   perc=y.Freq) %>%
            mutate(perc = 100*round(perc, digits))
        )

      data_cat <-
        data.frame(
          Variable = rep(cat_names, nlevs),
          suppressWarnings(bind_rows(data_cat)),
          stringsAsFactors = FALSE
        ) %>% # suppress coerce to char message
        rename(`%` = perc)    # otherwise lose symbol on bind rows
    } else {
      return(message('No categorical data.'))
    }
  } else {
    return(message('No categorical data.'))
  }

  # a check on a sensible max_levels
  if (any(max_levels > map_int(data_cat, n_distinct)))
    max_levels <- max(map_int(data_cat, n_distinct))

  data_cat <- data_cat %>%
    group_by(Variable) %>%
    top_n(n = max_levels, wt=`%`)

  if (isTRUE(sort_by_freq)) {
    # grouped arrange sorts the grouping variable too for no reason, which would
    # make visual inspection not as straightforward
    data_cat <- data_cat %>%
    mutate(Group = Group[order(`%`, decreasing = TRUE)],
           Frequency = Frequency[order(`%`, decreasing = TRUE)],
           `%` = sort(`%`, decreasing = TRUE))
    # arrange(desc(`%`), .by_group = TRUE) # why?
  }

  if (as_ordered) {
    return(
      data_cat %>%
        tidyr::nest(.key = 'data') %>%
        mutate(data = map(data, mutate, Group = factor(Group,
                                                       levels = unique(Group))))
    )
  }

  ungroup(data_cat)
}

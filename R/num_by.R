#' Summarize data by groups
#'
#' @description Provide common numeric summary information.
#'
#' @param data data frame
#' @param main_var the variable to be summarized. Multiple variables are
#'   supplied using the vars() function.
#' @param group_var the (optional) grouping variable.
#' @param perc_by_group when supplied a grouping variable for cat_by, do you
#'   want within group percentages also (default is TRUE)
#' @param sort_by_group when supplied a grouping variable for cat_by, do you
#'   want result sorted on the grouping variable? Default is TRUE.
#' @param digits Optional rounding. Default is 1.
#' @param extra See \link[tidyext]{num_summary}.
#'
#' @details The \code{num_by} function takes a numeric variable from a dataframe
#'   and provides sample size, mean, standard deviation, min, first quartile,
#'   median, third quartile, max, and number of missing values, possibly over a
#'   grouping variable.
#'
#'   It works in the dplyr style using unquoted (bare) variable names, using the
#'   \code{vars()} function if there is more than one variable.  If using a
#'   grouping variable, it will treat missing values as a separate group.
#'
#'   For \code{cat_by}, frequencies and percentage (out of total or group_var)
#'   are returned. Warnings are given if any of the main
#'   variables are more than 10 levels, or the data appears to be
#'   non-categorical. The group_var argument is essentially just to provide
#'   percentages based on a focal grouping variable out of all those supplied.
#'   Missing values are treated as an additional unique value.
#'
#'   Missing values are treated as separate groups, as it's often useful to
#'   explore the nature of the missingness. To avoid this, just use
#'   \code{na.omit} or \code{dplyr::drop_na} on the data frame first.
#'
#' @return data.frame/tibble with the corresponding summary statistics
#' @seealso describe_all
#' @importFrom stats quantile
#' @importFrom rlang is_quosures
#' @importFrom tidyr nest unnest
#' @importFrom dplyr quo_name
#' @importFrom purrr map_dfr
#' @examples
#' library(tidyext)
#' df1 <- data.frame(g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
#'                   g2 = sample(1:4, 50, replace = TRUE),
#'                   a = rnorm(50),
#'                   b = rpois(50, 10),
#'                   c = sample(letters, 50, replace=TRUE),
#'                   d = sample(c(TRUE, FALSE), 50, replace=TRUE)
#'                  )
#'
#'
#' num_by(df1, main_var = a)
#' num_by(df1, main_var = a, group_var = g2, digits=2)
#'
#' num_by(df1, main_var = dplyr::vars(a,b), group_var = g1, digits=1)
#'
#' cat_by(df1, main_var = g1, group_var = g2, digits=1)
#' cat_by(df1, main_var = dplyr::vars(g1,d), group_var = g2, perc_by_group=FALSE)
#'
#' @export
num_by <- function(data,
                   main_var,
                   group_var,
                   digits = 1,
                   extra = FALSE) {

  # for future reference; the tryCatch was just to make it easy to pass a single
  # variable name in lieu of using vars().
  # - variants of vars do not return the same class object as vars()
  #
  # Other stuff:
  # you can't do !!enquo(x); you have to do z = enquo(x); !!z or !(!enquo(x))
  # though the latter likely would only work within a function
  # in retrospect, mutate_at + distinct would have likely worked better, but
  # distinct won't take a vars input, because why would it?
  # perhaps tidyselect will have some useful documentation in the future

  # Initial checks ----------------------------------------------------------
  if (is.null(data))
    stop('No data to summarise.')
  if (nrow(data) == 0)
    stop('No data to summarise.')
  if (!inherits(data, 'data.frame'))
    stop('Need a data.frame type object.')


  # section to deal with single variable name versus vars() ------------------
  mv <- enquo(main_var)

  vars_check = grepl(rlang::expr_text(mv), pattern = 'vars')

  if (!vars_check)
    main_var <- quos(!!mv)

  # main_var class check: the following unwieldy bit is first just to check for
  # numeric-like variables passed to main_var.  More of it is just to be able to
  # pass a -x for main_var, as selecting in that fashion would include the
  # grouping variable, which we don't want to include in the class check; if the
  # usual selection case occurs (i.e. no - sign), then the group_var wouldn't be
  # selected, so trying to drop it would cause an error; purrr::map variants
  # just provided new ways to fail

  data = dplyr::ungroup(data)   # remove any previous grouping
  gv <- enquo(group_var)
  no_group = rlang::quo_is_missing(gv)

  if (no_group) {
    class_mv <- data %>%
      summarise_at(main_var, class) %>%
      unlist()
  } else {
    class_mv <- data %>%
      select_not(!!gv) %>%
      summarise_at(main_var, class) %>%
      unlist()
  }

  if (!all(class_mv %in% c('numeric', 'integer', 'logical')))
    stop('Non-numeric/logical variable detected.')

  nc <- length(class_mv)

  cnames <- names(select_at(data, main_var))

  # Main processing ---------------------------------------------------------

  if (!no_group) {

    #• grouped result ----------------------------------------------------------

    data <- data %>%
      select(!!!main_var, !!gv) %>%
      group_by(!!gv) %>%
      tidyr::nest() %>%
      mutate(result = purrr::map(data,
                                 ~purrr::map_dfr(., function(x)
                                   num_summary(x, digits, extra),
                                   .id='Variable')
                                 )
             ) %>%
      tidyr::unnest(result)

    # sometimes tidyr will return the list column of data
    if ('data' %in% names(data)) {
      data = select_not(data, `data`)
    }
  } else {

    #• non-grouped result ------------------------------------------------------

    data <- data %>%
      select_at(main_var) %>%
      purrr::map_df(num_summary, digits, extra) %>%
      mutate(Variable = cnames) %>%
      select(Variable, everything())
  }

  data
}



#' @rdname num_by
#' @export
#'
cat_by <- function(data,
                   main_var,
                   group_var,
                   digits=FALSE,
                   perc_by_group=TRUE,
                   sort_by_group=TRUE) {
  if (nrow(data) == 0 | is.null(data))
    stop('No data to summarise.')

  mv <- enquo(main_var)

  vars_check = grepl(rlang::expr_text(mv), pattern = 'vars')

  if (!vars_check)
    main_var <- quos(!!mv)

  data = dplyr::ungroup(data)   # remove any previous grouping

  class_mv <- data %>%
    select_at(main_var) %>%
    purrr::map(class) %>%
    unlist() %>%
    unique()

  nlevs <- data %>%
    summarise_at(main_var, funs(n_distinct(.))) %>%
    unlist()

  if (!all(class_mv  %in% c('character', 'factor', 'logical', 'ordered')))
    warning("Some of these do not appear to be character, factor/ordered,
  or logical variables.")

  if (any(nlevs > 10))
    warning("Greater than 10 levels found for at least one variable.
    This probably won't display well as a table.")

  # use of group var --------------------------------------------------------
  gv_exists <- !rlang::quo_is_missing(enquo(group_var))

  if (gv_exists) {

    gv <- enquo(group_var)
    main_var <- c(vars(!!gv), main_var)

    data <- data %>%
      select_at(main_var) %>%
      group_by_all() %>%
      summarise(N = n()) %>%     # Treat NA as a category -sum(is.na(!!mv))
      ungroup() %>%
      mutate(`% of Total` = 100*N/sum(N))

    if (perc_by_group) {
      varname <- dplyr::quo_name(gv)

      data <- data %>%
        group_by(!!gv) %>%
        mutate(gv_perc = 100*N/sum(N))

      colnames(data)[colnames(data) == 'gv_perc'] <- paste0('% of ', varname)
    }

    if (sort_by_group){
      data <- data %>%
        select(!!gv, everything()) %>%
        arrange(!!gv)
    }
  } else {
    # no group var --------------------------------------------------------
    data <- data %>%
      group_by_at(main_var) %>%
      summarise(N = n()) %>%     # Treat NA as a category -sum(is.na(!!mv))
      ungroup() %>%
      mutate(`% of Total` = 100*N/sum(N))

  }

  if (digits) data <- data %>% mutate_if(is.numeric, round, digits=digits)

  data
}

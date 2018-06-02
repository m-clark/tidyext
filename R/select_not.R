#' Don't select these unquoted columns passed as arguments.
#' @description Drop columns provided as unquoted names to arguments.
#' @param data The data set to work with.
#' @param ... Arguments as with \code{select}.
#' @details This function is not exported, as most should have no problems using
#'   basic \code{select} functionality for common needs.  However, trying to
#'   drop/deselect variables that started without quotes was a maddening
#'   exploration of tidyeval I feel no better about myself for having gone
#'   through. The problem is that something like following doesn't work:
#'

#'
#'   Here is
#'   \href{https://stackoverflow.com/questions/50001012/excluding-multiple-columns-based-on-unquote-splicing}{another
#'   solution} that I came across later that speaks directly to the problem, but
#'   is essentially the same solution.
#'
#' @return Whatever select would have done with a minus sign, if that had worked
#'   as it logically should.
#'
select_not <- function(data, ...) {
  not_these = names(select(data, ...))

  data %>%
    select(-one_of(not_these))
}

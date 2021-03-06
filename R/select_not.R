#' Don't select these unquoted columns passed as arguments. select(-!!var) 😲
#' @description Drop columns provided as unquoted names to arguments.
#' @param data The data set to work with.
#' @param ... Arguments as with \code{select}.
#' @details This function is not exported, as most should have no problems using
#'   basic \code{select} functionality for common needs.  However, trying to
#'   drop/deselect variables that started without quotes was a maddening
#'   exploration of tidyeval I feel no better about myself for having gone
#'   through. The problem is that something like following doesn't work:
#'
#'   \code{select(-!!var)} 😲
#'
#'   If you follow typical examples provided for tidy evaluation, you'll just
#'   get `invalid argument` errors.  I needed this functionality because an
#'   operation was to be performed on some columns, but not if some operations
#'   had been performed on others, and those others were passed as unquoted
#'   variables via \code{vars()}. See \link[tidyext]{pre_process}.
#'
#'   In the end, it was far easier to write a two line function than figure out
#'   the tidy eval approach that would have worked with less code. It literally
#'   just does:
#'
#'   \code{ not_these = names(select(data, ...)) }
#'
#'   \preformatted{data \%>\%
#'   select(-one_of(not_these)) }
#'
#'
#'   \href{https://stackoverflow.com/questions/45100518/remove-columns-the-tidyeval-way}{Lionel's
#'   answer on SO} shows how one \emph{could} do it.  He is a
#'   developer for RStudio, so one can assume it's a fairly spot on suggestion.
#'   Yet even that would need a bit of modification to work with a
#'   \code{vars()}, which I needed for multiple column entries to a single
#'   argument.  Here is what ultimately would work with tidyeval using that
#'   approach.
#'
#'   \preformatted{x = vars_input \%>\%
#'       map(function(sym) call("-", sym)) }
#'
#'   \preformatted{data \%>\%
#'   select(!!!x) }
#'
#'
#'   You then would need an '\code{x}' for every argument's input.  And no one reading the
#'   code would know what's going on, which goes against one of the goals for this
#'   package.  😞
#'
#'   Here is
#'   \href{https://stackoverflow.com/questions/50001012/excluding-multiple-columns-based-on-unquote-splicing}{another
#'   solution} that I came across later that speaks directly to the problem, but
#'   is essentially the same solution.
#'
#' @return Whatever select would have done with a minus sign, if that had worked
#'   as it logically should.
#' @keywords internal
select_not <- function(data, ...) {
  not_these <- names(select(data, ...))

  data %>%
    select(-one_of(not_these))
}

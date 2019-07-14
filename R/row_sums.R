#' Apply simple rowwise functions
#'
#' @description Get means, sums and possibly other functions for a select number
#'   of columns using \code{dplyr::select}-style column selection.
#'
#' @param data A data frame.
#' @param ... The columns to sum, take the mean of, etc. \emph{Required}.
#' @param .fun The function to apply.
#' @param na_rm Whether to remove \code{NA} values or not. Default is \code{FALSE}.
#' @param varname The column name of the sums means etc.
#'
#' @details Simple wrappers for applying rowwise operations only for selected
#'   columns within the tidyverse approach to data processing.  The
#'   \code{row_apply} function requires a function that already works on rows.
#'
#' @return A data frame with a new column representing the sums, means or
#'   whatever.
#'
#' @examples
#' library(tidyext)
#' library(dplyr)
#'
#' d = data.frame(x = 1:3, y = 4:6, z = 7:9)
#'
#' d  %>%
#'   row_sums(x:y)
#'
#' d  %>%
#'   row_means(matches('x|z'))
#'
#' row_apply(
#'   d ,
#'   everything(),
#'   .fun = function(x)
#'     apply(x, 1, paste, collapse = '')
#' )
#'
#' @export
row_sums <- function(data, ..., na_rm = FALSE, varname = 'sum') {
  dplyr::mutate(data, !!varname := rowSums(select(data, ...), na.rm = na_rm))
}

#' @export
#' @rdname row_sums
row_means <- function(data, ..., na_rm = FALSE, varname = 'mean') {
  dplyr::mutate(data, !!varname := rowMeans(select(data, ...), na.rm = na_rm))
}

#' @export
#' @rdname row_sums
row_apply <- function(data, ..., .fun, varname = 'var') {
  dplyr::mutate(data, !!varname := .fun(select(data, ...)))
}



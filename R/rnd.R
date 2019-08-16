#' Sensible rounding for printing.
#'
#' @param x A numeric vector
#' @param digits Digits to round to. Default is 3.
#' @details This is the rounding many are wanting for printing out results. For
#'   example, there will always be \code{digits} number of decimal places, and
#'   trailing zeros will be kept.
#' @return A character vector suitable for printed output.
#' @export
#'
#' @examples
#' library(tidyext)
#' set.seed(123)
#' rnd(rnorm(10))
#' rnd(1)
rnd = function(x, digits = 3) {
  if (! is.numeric(x))
    stop('Variable must be numeric.')

  format(round(x, digits), nsmall = digits)
}

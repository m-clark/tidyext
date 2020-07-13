#' Show head and tail simultaneously
#'
#' @param data The data frame to slice
#' @param n_slice How many rows of the head and tail you want. Default is 6.
#'
#' @details This will work on matrices and also not be limited to default tibble display.
#' @return A data frame of the first and last n_slice rows
#'
#' @export
#'
#' @examples
#'
#' library(tidyext)
#'
#' head_tail(mtcars)
#'
head_tail = function(data, n_slice = 6) {
  # initial checks
  if (is.matrix(data))
    data <- as.data.frame(data)

  if (!inherits(data, 'data.frame'))
    stop('Need a data frame.')

  # create the slicing index and add to result
  index = c(1:n_slice, (nrow(data) - n_slice + 1):nrow(data))

  data = data %>%
    dplyr::slice(index) %>%
    dplyr::mutate(index = index) %>%
    dplyr::select(index, everything())

  if (inherits(data, 'tbl_df') && n_slice * 2 > 20)
    data %>% print(n = nrow(.))
  else
    data
}

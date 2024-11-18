#' Fixed rounding
#'
#' @description Round numeric to fixed decimal.
#'
#' @param x numeric vector
#' @param k decimals (integer)
#'
#' @return rounded vector (character)
#'

round_k <- function(x, k = 2) {
  trimws(format(round(x, k), nsmall = k))
}

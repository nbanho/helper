#' @title Smart rounding
#'
#' @description Round vector without remainder.
#'
#' @param x numeric vector
#'
#' @return rounded numeric vector
#'

smart_round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

#' @title Distribution of epidemiological parameters
#'
#' @description Create discrete vector of continuous delay distribution.
#'
#' @param t time horizon
#' @param from0 if delay starts from zero
#' @param FUN delay distribution function
#' @param ... additional parameters to `FUN``
#'
#' @return a discrete vector of the delay distribution
#'

disc_dist <- function(t, from0, FUN, ...) {
  x <- seq(0, t)
  y <- length(x)
  if (from0) {
    y[1] <- FUN(0.5, ...)
    y[2] <- FUN(1.5, ...) - FUN(0.5, ...)
  } else {
    y[1] <- 0
    y[2] <- FUN(1.5, ...)
  }
  for (i in 3:length(x)) {
    y[i] <- FUN(x[i] + 0.5, ...) - FUN(x[i] - 0.5, ...)
  }
  return(y)
}

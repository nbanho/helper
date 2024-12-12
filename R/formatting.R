#' @title Rounding
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


#' @title Format statistics
#'
#' @description Format p-values in typical journal reporting syle.
#'
#' @param x numeric p-value
#'
#' @returned formatted p-value (character)
#'

format_pvalue <- function(x) {
  ifelse(
    x < .001, "p < 0.001",
    ifelse(x < 0.01, paste("p =", round(x, 3)),
      paste("p =", round(x, 2))
    )
  )
}

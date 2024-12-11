#' @title Moments matching for common statistical distributions
#'

lnorm_moments <- function(meanlog, sdlog) {
  m <- exp(meanlog + (1 / 2) * sdlog^2)
  s <- exp(meanlog + (1 / 2) * sdlog^2) * sqrt(exp(sdlog^2) - 1)
  return(list(mean = m, sd = s))
}

inv_lnorm_moments <- function(mean, sd) {
  m <- log(mean) - 0.5 * log((sd / mean)^2 + 1)
  s <- sqrt(log((sd / mean)^2 + 1))
  return(list(meanlog = m, sdlog = s))
}

gamma_moments <- function(shape, rate) {
  m <- shape / rate
  s <- sqrt(shape) / rate
  return(list(mean = m, sd = s))
}

inv_gamma_moments <- function(mean, sd) {
  s <- (mean / sd)^2
  r <- mean / (sd^2)
  return(list(shape = s, rate = r))
}

inv_beta_moments <- function(mean, sd) {
  variance <- sd^2
  alpha <- mean * ((mean * (1 - mean) / variance) - 1)
  beta <- (1 - mean) * ((mean * (1 - mean) / variance) - 1)
  list(alpha = alpha, beta = beta)
}

beta_moments <- function(alpha, beta) {
  m <- alpha / (alpha + beta)
  s <- sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
  return(list(mean = m, sd = s))
}


weibull_moments <- function(shape, scale) {
  mean <- scale * gamma(1 + 1 / shape)
  variance <- scale^2 * (gamma(1 + 2 / shape) - (gamma(1 + 1 / shape))^2)
  sd <- sqrt(variance)
  list(mean = mean, sd = sd)
}

inv_weibull_moments <- function(mean, sd) {
  shape <- (sd / mean)^(-1.086)
  scale <- mean / gamma(1 + 1 / shape)
  list(shape = shape, scale = scale)
}

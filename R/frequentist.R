#' @title Frequentist model pooling
#'
#' @description Pool estimates from list of fitted models using Rubin's rule.
#'
#' @param models list of fitted models
#' @param coef predictor/coefficient that should be pooled
#' @param pool_fun the function to pool estimates
#' @param var_fun the function to compute variance between estimates
#'
#' @return named vector with estimate, std_error and p_value
#'

rubins_rule <- function(models, coef, pool_fun = mean, var_fun = var) {
  # Filter failed models
  models <- Filter(Negate(is.null), models)

  # Estimates
  estimates <- map_dbl(models, ~ coef(.x)[coef])

  # Standard errors
  std_errors <- map_dbl(models, ~ sqrt(diag(vcov(.x)))[coef])

  # Filter NAs
  estimates <- estimates[!is.na(std_errors)]
  std_errors <- std_errors[!is.na(std_errors)]

  # Number of datasets
  m <- length(estimates)

  # Calculate the pooled estimate
  pooled_estimate <- pool_fun(estimates)

  # Calculate the within-imputation variance
  within_var <- pool_fun(std_errors^2)

  # Calculate the between-imputation variance
  between_var <- var_fun(estimates)

  # Calculate the total variance
  total_var <- within_var + (1 + 1 / m) * between_var

  # Calculate the pooled standard error
  pooled_std_error <- sqrt(total_var)

  # Calculate the p-value for the pooled estimate
  t_value <- pooled_estimate / pooled_std_error
  df <- m - 1
  p_value <- 2 * pt(-abs(t_value), df)

  # Return the results as a list
  c(
    "estimate" = pooled_estimate,
    "std_error" = pooled_std_error,
    "p_value" = p_value
  )
}

#' @param c1 coefficient name 1
#' @param c2 coefficient name 2

rubins_rule_covar <- function(models, c1, c2, pool_fun = mean) {
  # Filter failed models
  models <- Filter(Negate(is.null), models)

  # Estimates
  estimates_1 <- map_dbl(models, ~ coef(.x)[c1])
  estimates_2 <- map_dbl(models, ~ coef(.x)[c2])

  # Standard errors
  std_errors_1 <- map_dbl(models, ~ sqrt(diag(vcov(.x)))[c1])
  std_errors_2 <- map_dbl(models, ~ sqrt(diag(vcov(.x)))[c2])

  # Covariances
  within_cov <- map_dbl(models, ~ vcov(.x)[c1, c2])

  # Filter NAs
  estimates_1 <- estimates_1[!is.na(std_errors_1) & !is.na(std_errors_2)]
  estimates_2 <- estimates_2[!is.na(std_errors_1) & !is.na(std_errors_2)]
  within_cov <- within_cov[!is.na(std_errors_1) & !is.na(std_errors_2)]

  # Compute within-imputation covariance (W)
  W <- pool_fun(within_cov)

  # Compute mean estimates
  mean_est_1 <- pool_fun(estimates_1)
  mean_est_2 <- pool_fun(estimates_2)

  # Compute between-imputation covariance (B)
  if (identical(pool_fun, mean)) {
    m <- sum(!is.na(std_errors_1) & !is.na(std_errors_2))
    B <- sum((estimates_1 - mean_est_1) * (estimates_1 - mean_est_2)) / (m - 1)
    # Compute total covariance (T)
    WT <- W + (1 + 1 / m) * B
  } else {
    B <- pool_fun((estimates_1 - mean_est_1) * (estimates_1 - mean_est_2))
    # Compute total covariance (T)
    WT <- W + B
  }
  return(WT)
}


#' @title Feature importance
#'
#' @description Compute change in log-likelihood over null model relative to other models.
#'
#' @param models list of fitted models
#' @param null_model null model
#'
#' @details
#' Let LL_i be the log-likelihood of model 1 and 2.
#' Let LL_0 be the log-likelihood of the null model.
#' Then, DLL_i = (LL_0 - LL_i) are the differences.
#' Feature importance is contribution C_i = DLL_i / sum(DLL_i).
#' It can be interpreted as the realtive contribution of a model.
#'
#' @return relative changes in log-likelihood
#'

relative_contribution <- function(models, null_model) {
  ll <- map_dbl(models, ~ logLik(.x)[1])
  ll_null <- logLik(null_model)[1]
  dll <- (ll_null - ll)
  dll / sum(dll)
}

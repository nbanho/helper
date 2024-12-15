#' @title Frequentist model pooling
#'
#' @description Pool estimates from list of fitted models using Rubin's rule.
#'
#' @param models list of fitted models
#' @param coef predictor/coefficient that should be pooled
#'
#' @return named vector with estimate, std_error and p_value
#'

rubins_rule <- function(models, coef) {
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
  pooled_estimate <- mean(estimates)

  # Calculate the within-imputation variance
  within_var <- mean(std_errors^2)

  # Calculate the between-imputation variance
  between_var <- var(estimates)

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

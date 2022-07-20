#' @title get_CI
#'
#' @description Estimate confidence interval of the the c-statistic.
#'
#' @param logitc logit of the c-statistic (AUC) of a predictive model, calculated using the logit function
#' @param varlogitc variance of the logit of the c-statistic (AUC) of a predictive model, estimated using est_varlogitc
#' @param n Sample size in the dataset used to generate the c-statistic
#' @param alpha Desired significance level
#'
#' @return Confidence interval of the c-statistic
#' @export

get_CI <- function(logitc, varlogitc, n, alpha = 0.05){

  t_crit <- qt((1-alpha/2), n-1)
  #z <- pnorm(1-alpha/2)
  exp_high <- logitc + t_crit * varlogitc
  exp_low <- logitc - t_crit * varlogitc

  CI_low <- invlogit(exp_low)
  CI_high <- invlogit(exp_high)

  return(c(CI_low,CI_high))
}

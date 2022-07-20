#' @title get_CI_vec
#'
#' @description Estimate confidence intervals of the the c-statistic for multiple studies at once. Inputs are arrays representing corresponding values for each study.
#'
#' @param logitc logit of the c-statistic (AUC) of a predictive model, calculated using the logit function
#' @param SElogitc standard error of the logit of the c-statistic (AUC) of a predictive model, estimated using est_SElogitc
#' @param n Sample size in the dataset used to generate the c-statistic
#' @param alpha Desired significance level
#'
#' @return Lower bound of confidence interval of the c-statistic
#' @export


get_CI_vec <- function(logitc, SElogitc, n, alpha = 0.05){

  CI_low <- mapply(get_CI_low,logitc, SElogitc^2,n, alpha)
  CI_high <- mapply(get_CI_high,logitc, SElogitc^2,n, alpha)

  CI <- cbind(CI_low, CI_high)

  return(CI)
}

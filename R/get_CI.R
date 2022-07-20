#' @title get_CI
#'
#' @description Testing 123
#'
#' @param x A value between 0 and 1
#'
#' @return The logit of x
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

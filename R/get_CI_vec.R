#' @title get_CI_vec
#'
#' @description Testing 123
#'
#' @param x A value between 0 and 1
#'
#' @return The logit of x
#' @export


get_CI_vec <- function(logitc, SElogitc, n, alpha = 0.05){

  CI_low <- mapply(get_CI_low,logitc, SElogitc^2,n, alpha)
  CI_high <- mapply(get_CI_high,logitc, SElogitc^2,n, alpha)

  CI <- cbind(CI_low, CI_high)

  return(CI)
}

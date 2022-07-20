#' @title est_varc
#'
#' @description Testing 123
#'
#' @param x A value between 0 and 1
#'
#' @return The logit of x
#' @export

est_varc <- function(c, n_sample, n_events){
  N <- n_sample
  n <- n_events

  Q1 <- c / (2-c)
  Q2 <- 2 * c^2 / (1 + c)
  var <- c * (1-c) + (n - 1) * (Q1 - c^2) + (N - n - 1) * (Q2 - c^2) / (n * (N- n))

  return(var)
}

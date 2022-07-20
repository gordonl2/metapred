#' @title est_varlogitc
#'
#' @description Testing 123
#'
#' @param x A value between 0 and 1
#'
#' @return The logit of x
#' @export

est_varlogitc <- function(c,num_events, num_non_events){

  logitc <- logit(c)
  m <- num_non_events
  n <- num_events

  m_star <- 0.5*(m+n) - 1
  print(m_star)

  num1 <- m_star * ((1-c) / (2-c))
  num2 <- (m_star * c) / (1+c)
  num <- (1+num1+num2)
  denom <- (m*n*c*(1-c))
  Varlogitc <- num / denom

  return(varlogitc)
}

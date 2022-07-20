#' @title est_SElogitc
#'
#' @description Estimate standard error of the logit of the c-statistic
#'
#' @param c C-statistic (AUC) of a predictive model, value between 0 and 1
#' @param num_events Number of events in the dataset used to generate the c-statistic
#' @param num_non_events Number of non-events in the dataset used to generate the c-statistic (number of cases - number of events)
#'
#' @return Standard error of the logit of the c-statistic
#' @export

est_SElogitc <- function(c, num_events, num_non_events){

  logitc <- logit(c)
  m <- num_non_events
  n <- num_events

  m_star <- 0.5*(m+n) - 1
  print(m_star)

  num1 <- m_star * ((1-c) / (2-c))
  num2 <- (m_star * c) / (1+c)
  num <- (1+num1+num2)
  denom <- (m*n*c*(1-c))
  SElogitc <- sqrt(num / denom)

  return(SElogitc)
}

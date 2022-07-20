#' @title est_varc
#'
#' @description Estimate variance of the c-statistic based on the sample size and number of events.
#'
#' @param c C-statistic (AUC) of a predictive model, value between 0 and 1
#' @param num_events Number of events in the dataset used to generate the c-statistic
#' @param num_non_events Number of non-events in the dataset used to generate the c-statistic (number of cases - number of events)
#'
#' @return Variance of the c-statistic
#' @export

est_varc <- function(c,num_events, num_non_events){
  N <- num_events + num_non_events
  n <- num_events

  Q1 <- c / (2-c)
  Q2 <- 2 * c^2 / (1 + c)
  var <- c * (1-c) + (n - 1) * (Q1 - c^2) + (N - n - 1) * (Q2 - c^2) / (n * (N- n))

  return(var)
}

#' @title est_SEc
#'
#' @description Estimate standard error of the c-statistic
#'
#' @param c C-statistic (AUC) of a predictive model, value between 0 and 1
#' @param num_events Number of events in the dataset used to generate the c-statistic
#' @param num_non_events Number of non-events in the dataset used to generate the c-statistic (number of cases - number of events)
#'
#' @return Standard error of the c-statistic
#' @export

est_SEc <- function(c, num_events, num_non_events){
  m <- num_non_events
  n <- num_events

  m_star <- 0.5*(m+n) - 1

  SEc <- sqrt((c * (1-c) * (1+ (n * (1-c) / (2-c)) + ((m*c) / (1+c)))) / (m*n))

  return(SEc)
}

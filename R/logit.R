#' @title logit
#'
#' @description Logit function
#'
#' @param x A value between 0 and 1
#'
#' @return The logit of x
#' @export


logit <- function(x){
  logitx <- log(x / (1-x))
  return(logitx)
}

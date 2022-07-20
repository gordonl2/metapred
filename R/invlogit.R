#' @title invlogit
#'
#' @description Inverse of the logit function
#'
#' @param x A continuous value
#'
#' @return The inverse logit, a value between 0 and 1
#' @export

invlogit <- function(logitx){
  x <- 1 / (1 + exp(-logitx))

  return(x)
}

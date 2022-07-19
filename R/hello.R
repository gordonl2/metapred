#' @title hello
#'
#' @description Testing 4 5 6
#'
#' @param x A data set object
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export

hello <- function(x){
  logitx <- log(x / (1-x))
  return(logitx)
}

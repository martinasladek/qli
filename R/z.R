
#' Convert a vector into Z scores
#'
#' @param x A numeric vector
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,5)
#' z(x)
z <- function(x){
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}

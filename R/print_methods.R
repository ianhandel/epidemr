#' Print method for epi_2by2 class
#'
#' A technology test rather than a final product
#'
#'
#' @param x An epi_2by2 object
#' @param ... Other arguments
#' @examples
#' head(mtcars)
#' tab <- epi_2by2(mtcars, hp>100, am==1)
#' print(tab)
#' @export


print.epi_2by2 <- function(x, ...){
  assertthat::assert_that(class(x) == "epi_2by2")

  print(x[["table"]])
}

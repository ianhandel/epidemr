#' Print method for epi_2by2 class
#'
#' A technology test rather than a final product
#'
#'
#' @param object An epi_2by2 object
#' @examples
#' head(mtcars)
#' epi_2by2(mtcars, hp>100, am==1)
#' print(tab)


print.epi_2by2 <- function(object){
  assertthat::assert_that(class(object) == "epi_2by2")

  print(object[["table"]])
}

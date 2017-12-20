#' Two by two table summary
#'
#' A technology test rather than a final product
#'
#' Summary method for epi_2by2 class
#'
#' This needs A LOT of work
#'
#' @param object An epi_2by2 object
#' @param ... Other arguments
#' @return Returns a sumamry of aepi_2by2 object.
#' @export
#' @examples
#' head(mtcars)
#' epi <- epi_2by2(mtcars, hp>100, am==1)
#' summary(epi)


summary.epi_2by2 <- function(object, ...) {
  purrr::map(object, I)
}

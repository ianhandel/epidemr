#' Two by two table plot
#'
#' A technology test rather than a final product
#'
#' Introduce S3 class epi and explore printing and plotting methods
#'
#' @param object An epi_2by2 object
#' @examples
#' tab <- epi_2by2(mtcars, gear, carb)
#' qplot(tab)

qplot <- function(object){
  tbl <- dplyr::as_tibble(object[["table"]])
}

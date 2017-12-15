#' Two by two table plot
#'
#' A technology test rather than a final product
#'
#' Introduce S3 class epi and explore printing and plotting methods
#'
#' This needs A LOT of work
#'
#' @param object An epi_2by2 object
#' @param colours A palette of four colours
#' @examples
#' head(mtcars)
#' tab <- epi_2by2(mtcars, hp>100, am==1)
#' qplot(tab)

qplot <- function(object){
  plot(object[["table"]],
       main = "",
       col = viridis::viridis(n = 2, alpha = 0.75, begin = 0.15, end = 0.3))
}


#' Two by two table plot
#'
#' A technology test rather than a final product
#'
#' Introduce S3 class epi and explore printing and plotting methods
#'
#' This needs A LOT of work
#'
#' @param x An epi_2by2 object
#' @param ... Other arguments
#' @export
#' @examples
#' head(mtcars)
#' tab <- epi_2by2(mtcars, hp>100, am==1)
#' plot(tab)


plot.epi_2by2 <- function(x,...){
  graphics::plot(x[["table"]],
       main = "",
       col = viridis::viridis(n = 2, alpha = 0.75, begin = 0.15, end = 0.3))
}


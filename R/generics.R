#' Tidy the result of a test into a summary data.frame
#'
#' This from BROOM
#'
#' The output of tidy is always a data.frame with disposable row names. It is
#' therefore suited for further manipulation by packages like dplyr, reshape2,
#' ggplot2 and ggvis.
#'
#' @param x An object to be converted into a tidy data.frame
#' @param ... extra arguments
#'
#' @return a data.frame
#'
#' @export
tidy <- function(x, ...) UseMethod("tidy")

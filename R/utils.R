#' Utility function to convert TRUE/FALSE to positive/negative
#'
#' @param x Input vector
#'


TF_to_posneg <- function(x) {
  assertthat::see_if(is.logical(x))
  res <- dplyr::case_when(
    x ~ "positive",
    !x ~ "negative"
  )
  res <- factor(res, levels = c("positive", "negative"))
  res
}

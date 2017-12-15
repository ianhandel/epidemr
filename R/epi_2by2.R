#' Two by two table analysis
#'
#' A technology test rather than a final product
#'
#' Introduce S3 class epi and explore printing and plotting methods
#'
#' @param df A dataframe
#' @param outcome The unquoted column name for outcome.
#' @param exposure The unquoted column name for exposure
#' @return Returns an epi_2by2 object.
#' @examples
#' epi_2by2(mtcars, gear, carb)

epi_2by2 <- function(df, outcome, exposure){
  outcome <- rlang::enquo(outcome)
  exposure <- rlang::enquo(exposure)
  tbl <- table(df[[rlang::quo_text(outcome)]],
               df[[rlang::quo_text(exposure)]])
  col_names <- c(rlang::quo_text(outcome), rlang::quo_text(exposure))
  return(structure(list(table = tbl), class = "epi_2by2"))
}




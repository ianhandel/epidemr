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

#' Utility function to convert df and columns to matrix for epiR functions
#'
#' @param df dataframe to source 2 x 2 table
#' @param outcome The outcome column  (unquoted) or expression
#' @param exposure The exposure column (unquoted) or expresssion
#' @param outcome_labels A character vector, length 2, of outcome (positive, negative)
#' @param exposure_labels A character vectoe, length 2, of exposure (positive, negative)
#' @export
#' @examples
#' head(mtcars)
#' tab <- epi_table(mtcars, am == 1, cyl == 4)
#' epiR::epi.2by2(tab, method = "cross.sectional")
#' epi_2by2(tab, method = "cross.sectional")
#' epi_2by2(mtcars, am == 1, cyl == 4, method = "cross.sectional")

epi_table <- function(df,
                      outcome, exposure,
                      outcome_labels = c("diseased","healthy"),
                      exposure_labels = c("exposed", "unexposed")){

  assertthat::assert_that(any(class(df) %in% c("data.frame")))

  outcome <- rlang::enquo(outcome)
  exposure <- rlang::enquo(exposure)

  df <- dplyr::mutate(
    df,
    ..outcome.. = !! outcome,
    ..exposure.. = !! exposure
  )

  assertthat::assert_that(
    is.logical(df[["..outcome.."]]),
    msg = "Outcome must be TRUE/FALSE"
  )
  assertthat::assert_that(
    is.logical(df[["..exposure.."]]),
    msg = "Exposure must be TRUE/FALSE"
  )

  tab <- matrix(table(
    df[["..exposure.."]],
    df[["..outcome.."]]
  ), 2, 2,
  dimnames = list(exposure_labels, outcome_labels))

  tab
}

#' Wrap data from a dataframe and send to epiR::epi.tests
#'
#' A wrapper for epirR::epi.tests  called with a data.frame and
#' unquoted columns or expressions.
#'
#' @param df A dataframe
#' @param test The unquoted column name for test results
#' @param goldstandard The unquoted column name for gold standard test results
#' @param conf_level Probability for confidence interval calculations
#' @return Returns an epi.tests
#' @export
#' @examples
#' head(mtcars)
#' epi_tests(mtcars, am==1, cyl==4, conf_level = 0.95)


epi_tests <- function(df,
                      test,
                      goldstandard,
                      conf_level = 0.95) {
  test <- rlang::enquo(test)
  goldstandard <- rlang::enquo(goldstandard)

  df <- dplyr::mutate(
    df,
    ..test.. = !! test,
    ..goldstandard.. = !! goldstandard
  )

  assertthat::assert_that(is.logical(df[["..test.."]]),
                          msg = "Test must be TRUE/FALSE")
  assertthat::assert_that(is.logical(df[["..goldstandard.."]]),
                          msg = ".Goldstandard must be TRUE/FALSE")

  tab <- table(
    TF_to_posneg(df[["..test.."]]),
    TF_to_posneg(df[["..goldstandard.."]])
  )

  epiR::epi.tests(tab)
}

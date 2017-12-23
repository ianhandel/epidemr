#' Wrap data from a dataframe and send to epiR::epi.tests
#'
#' A wrapper for epirR::epi.tests normally called with a data.frame and
#' unquoted columns or expressions. Can also be called with an epi_twobytwo object (generated with twobytwo helper function) to allow direct entry of a table of results.
#'
#' @param x A dataframe or epi_two_by_two object
#' @param test The unquoted column name for test results
#' @param goldstandard The unquoted column name for gold standard test results
#' @param conf_level Probability for confidence interval calculations
#' @return Returns an epi.tests
#' @export
#' @examples
#' head(mtcars)
#' epi_tests(mtcars, mpg < 25, cyl > 4, conf_level = 0.95)


epi_tests <- function(x,
                      test = NULL,
                      goldstandard = NULL,
                      conf_level = 0.95) {

  assertthat::assert_that(any(class(x) %in% c("data.frame",
                                              "tbl",
                                              "epi_twobytwo")),
                          msg = "x must be a dataframe or epi_twobytwo object")

  if ("epi_twobytwo" %in% class(x)){
    return(epiR::epi.tests(as.matrix(with(x, c(TP, FN, FP, TN)),
                                     conf.level = conf_level)))
  }

  df <- x

  test <- rlang::enquo(test)
  goldstandard <- rlang::enquo(goldstandard)

  df <- dplyr::mutate(
    df,
    ..test.. = !! test,
    ..goldstandard.. = !! goldstandard
  )

  assertthat::assert_that(
    is.logical(df[["..test.."]]),
    msg = "Test must be TRUE/FALSE"
  )
  assertthat::assert_that(
    is.logical(df[["..goldstandard.."]]),
    msg = ".Goldstandard must be TRUE/FALSE"
  )

  tab <- table(
    TF_to_posneg(df[["..goldstandard.."]]),
    TF_to_posneg(df[["..test.."]])
  )

  epiR::epi.tests(tab, conf.level = conf_level)
}

#' Make an epi_twobytwo object for epi_tests
#'
#' A helper function for epi_tests that allows entry of summary data
#'
#' @param a The TRUE POSITIVE count
#' @param b The FALSE POSITIVE count
#' @param c The FALSE NEGATIVE count
#' @param d The TRUE NEGATIVE count
#' @return Returns an epi.tests
#' @export
#' @examples

#' epi_tests(twobytwo(12, 14, 1, 5))

twobytwo <- function(a, b, c, d){
  structure(list(TP = a, FP = b, FN = c, TN = d), class = "epi_twobytwo")
}

#' Wrap data from a dataframe and send to epiR::epi.tests
#'
#' A wrapper for epirR::epi.tests normally called with a data.frame and
#' unquoted columns or expressions. Can also be called with a numeric vector length 4 to allow direct entry of 2 x 2 table data
#'
#' @param x A dataframe or numeric vector length 4
#' @param test The unquoted column name for test results
#' @param goldstandard The unquoted column name for gold standard test results
#' @param ... Other parameters passed on to epiR::epi.tests
#' @return Returns an epi.tests
#' @export
#' @examples
#' head(mtcars)
#' epi_tests(mtcars, mpg < 25, cyl > 4, conf.level = 0.95)


epi_tests <- function(x,
                      test = NULL,
                      goldstandard = NULL,
                      ...) {

  assertthat::assert_that(any(class(x) %in% c("data.frame",
                                              "tbl") |
                                class(x) == "numeric" & length(x) == 4 |
                                class(x) == "table"),
                          msg = "x must be a dataframe, table or numeric vector length 4")

  if (class(x) == "numeric" | class(x) == "table"){
    return(epiR::epi.tests(as.matrix(x, 2, 2), ...))
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

  epiR::epi.tests(tab, ...)
}



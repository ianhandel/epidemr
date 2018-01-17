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
#' @details If using 4 numbers epi_test(c(a, b, c, d), ...)
#' @details Format is:
#' @details -----------	----------	-------------
#' @details   Disease   +	          - or time at risk
#' @details -----------	----------	-------------
#' @details  Test +     a            b
#' @details  Test -	    c            d
#' @details -----------	----------	-------------
#' @details   Total	    a + c	      b + d
#' @details -----------	----------	-------------
#' @export
#' @examples
#' head(mtcars)
#' epi_tests(mtcars, mpg < 25, cyl > 4, conf.level = 0.95)
#' res <- epi_tests(c(80, 1, 20, 99), conf.level = 0.95)



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
    return(epiR::epi.tests(matrix(x, 2, 2), ...))
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

#' Tidy the output from epiR::epi.tests into a tibble
#'
#'
#' @param x An epi.tests object
#' @param ... other arguments - ignored
#' @return Returns a tibble
#' @export
#' @examples
#' head(mtcars)
#' res <- epi_tests(mtcars, mpg < 25, cyl > 4, conf.level = 0.95)
#' tidy(res)

tidy.epi.tests <- function(x, ...){
  res <- dplyr::bind_rows(x$rval, .id = "variable")
  dplyr::mutate(res, conf.level = x$conf.level)
}

#' Plot the output from epiR::epi.tests
#'
#'
#' @param x An epi.tests object
#' @param ... other arguments - ignored
#' @return Returns a ggplot
#' @export
#' @examples
#' head(mtcars)
#' res <- epi_tests(mtcars, am == 1, cyl == 4, conf.level = 0.95)
#' plot(res)

plot.epi.tests <- function(x, ...){

  df <- dplyr::bind_rows(x$rval[c("se","sp")], .id = "parameter")
  g <- ggplot2::ggplot(df, ggplot2::aes_(x = ~parameter,
                                  y = ~est,
                                  ymin = ~lower,
                                  ymax = ~upper)) +
    ggplot2::geom_errorbar(width = 0.1) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Parameter",
                  y = "Estimate",
                  caption = paste0(x$conf.level * 100, "% CI")) +
    ggplot2::ylim(c(0,1))
  g
}




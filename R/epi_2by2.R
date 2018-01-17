#' Wrap data from a dataframe and send to epiR::epi.2by2 (or pass through table)
#'
#' A wrapper for epirR::epi.2by2 normally called with a data.frame and
#' unquoted columns or expressions. Can also be called with a numeric vector length 4 to allow direct entry of 2 x 2 table data
#'
#' @param x A dataframe or numeric vector length 4
#' @param outcome The unquoted column name for test results
#' @param exposure The unquoted column name for exposure column
#' @param ... Other parameters passed on to epiR::epi.2by2
#' @return Returns an epi.2by2 object
#' @details If using 4 numbers epi_2by2(a, b, c, d, ...)
#' @details Format is:
#' @details -----------	----------	-------------
#' @details   Disease   +	          - or time at risk
#' @details -----------	----------	-------------
#' @details   Expose +	a            b
#' @details   Expose -	c            d
#' @details -----------	----------	-------------
#' @details   Total	    a + c	      b + d
#' @details -----------	----------	-------------
#' @export
#' @examples
#' head(mtcars)
#' epi_2by2(mtcars, am == 1, cyl == 4, conf.level = 0.95)
#' epi_2by2(c(12, 34, 1, 6), conf.level = 0.95)


epi_2by2 <- function(x,
                      outcome = NULL,
                      exposure = NULL,
                      ...) {

  assertthat::assert_that(any(class(x) %in% c("data.frame",
                                              "tbl") |
                                class(x) == "numeric" & length(x) == 4 |
                                class(x) == "table" |
                                class(x) == "matrix"),
                          msg = "x must be a dataframe, table or numeric vector length 4")

  if (class(x) == "numeric" | class(x) == "table" | class(x) == "matrix"){
    return(epiR::epi.2by2(matrix(x, 2, 2, byrow = TRUE), ...))
  }

  df <- x

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

  tab <- table(
    TF_to_posneg(df[["..exposure.."]]),
    TF_to_posneg(df[["..outcome.."]])
  )

  epiR::epi.2by2(tab, ...)
}


#' Tidy epi.2by2 output to give data.frame of estimates and CI
#'
#' @param x An epi.2by2 object
#' @param ... other arguments (ignored)
#' @export
#' @example res <- epi_2by2(mtcars, am == 1, cyl == 4, conf.level = 0.95)
#' @example tidy(res)


tidy.epi.2by2 <- function(x, ...){
  assertthat::assert_that(class(x) == "epi.2by2",
                          msg = "x must be an epi.2by2 object")
  res <- x[["res"]]

  res <- purrr::keep(res, ~identical(names(.x), c("est", "lower", "upper")))

  res <- dplyr::bind_rows(res, .id = "variable")

  dplyr::mutate(res, conf.level = x$conf.level)
}

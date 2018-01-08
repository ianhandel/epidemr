#' Binomial estimates from dataframe column
#'
#' A wrapper for binom::binom.confint  called with a data.frame and
#' unquoted columns or expressions.
#'
#' Given a column of a dataframe (and 'positive' level) generate a
#' binomial proportion estimate and confidence intervals
#'
#' @param x A dataframe or number of sucesses
#' @param y An expression evaluating to TRUE/FALSE or number of trials
#' @param conf_level Probability for confidence interval calculations
#' @param methods Confidence interval method see ?binom::binom.confint
#' @param digits Rounding digits
#' @param ... Additional arguments for binom::binom.confint "bayes" method
#' @return Returns an epi_binom object - this includes a variable for conf_level.
#' @export
#' @examples
#' head(mtcars)
#' epi_binom(mtcars, cyl==4, conf_level = 0.95, methods = "all")
#' epi_binom(7, 10, conf_level = 0.95, methods = "all")


epi_binom <- function(x,
                      y,
                      conf_level = 0.95,
                      methods = "all",
                      digits,
                      ...){


  if(length(x) == 1  & !is.list(x)){
    res <- binom::binom.confint(x = x,
                                n = y,
                                conf.level = conf_level,
                                methods = methods,
                                ...)
  }else{

  var <- rlang::enquo(y)

  x <- dplyr::mutate(x, ..var = !!var)

  var_vec <- x[["..var"]]

  col_name <- rlang::quo_text(var)

  res <- binom::binom.confint(x = sum(var_vec),
                              n = sum(!is.na(var_vec)),
                              conf.level = conf_level,
                              methods = methods,
                              ...)
  }
  # add conf_level as variable
  res <- dplyr::mutate(res,
                       conf_level = conf_level)

  res <- dplyr::rename(res, proportion = mean)

  if(!missing(digits)){
    res <- tibble::as_tibble(purrr::map_if(res,
                                           purrr::is_bare_numeric,
                                           round, digits))
  }

  res <- structure(tibble::as.tibble(res),
                   class = c("epi_binom", "tbl_df", "tbl", "data.frame"))
  return(res)
}

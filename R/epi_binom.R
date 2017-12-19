#' Binomial estimates from dataframe column
#'
#' A wrapper for binom::binom.confint  called with a data.frame and
#' unquoted columns or expressions.
#'
#' Given a column of a dataframe (and 'positive' level) generate a
#' binomial proportion estimate and confidence intervals
#'
#' @param df A dataframe
#' @param var The unquoted column name for variable
#' @param var_positive The "level" for a positive / of interest outcome
#' @param conf_level Probability for confidence interval calculations
#' @param methods Confidence interval method see ?binom::binom.confint
#' @param ... Additional arguments for binom::binom.confint "bayes" method
#' @return Returns an epi_binom object - this includes a variable for conf_level.
#' @export
#' @examples
#' head(mtcars)
#' epi_binom(mtcars, cyl, 4, conf_level = 0.95, methods = "all")


epi_binom <- function(df,
                      var,
                      var_positive = TRUE,
                      conf_level = 0.95,
                      methods = "all",
                      ...){

  var <- rlang::enquo(var)

  df <- dplyr::mutate(df, ..var = !!var)

  var_vec <- df[["..var"]]

  col_name <- rlang::quo_text(var)

  res <- binom::binom.confint(x = sum(var_vec == var_positive),
                              n = sum(!is.na(var_vec)),
                              conf.level = conf_level,
                              methods = methods,
                              ...)
  # add conf_level as variable
  res <- mutate(res, conf_level = conf_level)

  res <- structure(tibble::as.tibble(res),
                   class = c("epi_binom", "tbl_df", "tbl", "data.frame"))
  return(res)
}

#' Incidence rate estimates from dataframe column
#'
#' A wrapper for epiR::epi.conf  called with a data.frame and
#' unquoted columns or expressions.
#'
#' Given a column of a incidence and time at risk generate an
#' estimate and confidence intervals
#'
#' @param df A dataframe
#' @param event The unquoted event count column
#' @param time The unquoted tiem at risk column
#' @param conf_level Probability for confidence interval calculations
#' @param method The method to send to epiR::epi.conf
#' @param digits Rounding digits
#' @return Returns an epi_inc object - this includes a variable for conf_level.
#' @export
#' @examples
#' head(mtcars)
#' epi_inc(mtcars, cyl, hp, conf_level = 0.95)


epi_inc <- function(df,
                      event,
                      time,
                      conf_level = 0.95,
                      method = "exact",
                      digits){

  event <- rlang::enquo(event)
  time <- rlang::enquo(time)

  df <- dplyr::mutate(df, ..event = !!event)
  df <- dplyr::mutate(df, ..time = !!time)

  event_vec <- df[["..event"]]
  time_vec <- df[["..time"]]

  col_names <- c(rlang::quo_text(event), rlang::quo_text(time))

  res <- epiR::epi.conf(matrix(c(event_vec, time_vec), ncol = 2),
                        ctype="inc.rate",
                        conf.level = conf_level,
                        method = method)

  res <- dplyr::mutate(res,
                       conf_level = conf_level)

  if(!missing(digits)){
    res <- tibble::as_tibble(purrr::map_if(res,
                                           purrr::is_bare_numeric,
                                           round, digits))
  }

  res <- structure(tibble::as.tibble(res),
                   class = c("epi_inc", "tbl_df", "tbl", "data.frame"))
  return(res)
}

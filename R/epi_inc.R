#' Estimate incidence rate
#'
#'
#' Wrap data from a dataframe and send to epiR::epi.conf (or pass through table).
#' To calculate an incidence rate using cases and time at risk
#'
#' @param x A dataframe or number of cases
#' @param y See details
#' @param z See details
#' @param conf_level confidence level for estimates
#' @param methods estimation methods for CI to be passes to epiR::epi_conf
#' @param ... Other parameters passed on to epiR::epi.conf
#' @return Returns a tibble
#' @details If used with a dataframe use epi_inc(dataframe, cases-expression, time_at_risk-column, ...)
#' @details If used with summary data use epi_inc(number-of-cases, time-at-risk, ...)
#' @export
#' @examples
#' head(mtcars)
#' # using a dataframe...
#' epi_inc(mtcars, cyl == 4, disp, conf_level = 0.95)
#' # using summary data
#' epi_inc(70, 1010, conf_level = 0.95)


epi_inc <- function(x,
                    y,
                    z = NULL,
                    conf_level = 0.95,
                    methods = "exact",
                    ...) {
  if ("data.frame" %in% class(x)) {
     cases <- rlang::enquo(y)
     time_at_risk <- rlang::enquo(z)

     x <- dplyr::mutate(x,
                         ..cases = !!cases,
                         ..time_at_risk = !!time_at_risk)
     n_cases <- sum(x[["..cases"]])
     time_at_risk <- sum(x[["..time_at_risk"]])
     res <- epiR::epi.conf(
       matrix(c(n_cases, time_at_risk), nrow = 1),
       conf.level = conf_level,
       ctype = "inc.rate",
       method = methods,
       ...
     )
  } else {
    res <- epiR::epi.conf(
      matrix(c(x, y), nrow = 1),
      conf.level = conf_level,
      ctype = "inc.rate",
      method = methods,
      ...
    )
  }
  dplyr::mutate(res, conf_level = conf_level)
}

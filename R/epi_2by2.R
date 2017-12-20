#' Two by two table analysis
#'#'
#' Given two columns of a dataframe (and 'positive' levels) generate a 2 x 2
#' table and appropriate summary statsitics
#'
#' @param df A dataframe
#' @param outcome The unquoted column name for outcome.
#' @param exposure The unquoted column name for exposure
#' @param study_type The study type "cross", "cohort" or "case-control"
#' @param outcome_positive The "level" for a positive / of interest outcome
#' @param exposure_positive The "level" for a positive / of interest exposure
#' @param conf_level Probability for confidence interval calculations
#' @return Returns an epi_2by2 object.
#' @export
#' @examples
#' head(mtcars)
#' epi_2by2(mtcars, hp>100, am==1)




epi_2by2 <- function(df,
                     outcome,
                     exposure,
                     outcome_positive = TRUE,
                     exposure_positive = TRUE,
                     method = "cohort.count",
                     conf_level = 0.95,
                     homogeneity = "breslow.day",
                     outcome_format = "as.columns"){

  outcome <- rlang::enquo(outcome)
  exposure <- rlang::enquo(exposure)

  df <- dplyr::mutate(df, ..outcome = !!outcome)
  df <- dplyr::mutate(df, ..exposure = !!exposure)
  df <- dplyr::mutate_at(df, dplyr::vars(..outcome, ..exposure),
                         ~ factor(dplyr::case_when(.x == FALSE ~ "negative",
                                                          .x == TRUE ~ "positive",
                                                          TRUE ~ NA_character_),
                                        levels = c("positive", "negative")))

  outcome_vec <- df[["..outcome"]]
  exposure_vec <- df[["..exposure"]]

  col_names <- c(rlang::quo_text(outcome), rlang::quo_text(exposure))

  res <- epiR::epi.2by2(table(outcome_vec,
                              exposure_vec),
                        method = method,
                        homogeneity = homogeneity,
                        conf.level = conf_level,
                        outcome = outcome_format)

  return(res)
}




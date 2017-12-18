#' Two by two table analysis
#'
#' A technology test rather than a final product
#'
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
#' @examples
#' head(mtcars)
#' epi_2by2(mtcars, hp>100, am==1)

# TODO for output - inspired by EpiTools from Ausvet
# 2 x 2 table! DONE
# expected values 2 x 2 DONE
# Overall Incidence/Prevalence DONE
# Incidence/Prevalence in Exposed
# Incidence/Prevalence in Unexposed
# Odds Ratio
# Relative Risk (Cross-sectional study)
# Attributable Risk
# Attributable fraction in Exposed
# Population Attributable Risk
# Population Attributable Fraction
# Tests - statistic and p-value
# Uncorrected Chi-square
# Yates corrected Chi-square
# Fisher's exact test (2-tailed)
# Fisher's exact test (1-tailed)
# McNemar's test (paired data)



epi_2by2 <- function(df,
                     outcome,
                     exposure,
                     study_type = "cross",
                     outcome_positive = TRUE,
                     exposure_positive = TRUE,
                     conf_level = 0.95){

  outcome <- rlang::enquo(outcome)
  exposure <- rlang::enquo(exposure)

  df <- dplyr::mutate(df, ..outcome = !!outcome)
  df <- dplyr::mutate(df, ..exposure = !!exposure)

  outcome_vec <- df[["..outcome"]]
  exposure_vec <- df[["..exposure"]]

  assertthat::assert_that(length(unique(outcome_vec)) == 2)
  assertthat::assert_that(length(unique(exposure_vec)) == 2)

  col_names <- c(rlang::quo_text(outcome), rlang::quo_text(exposure))

  res <- calc_2by2(outcome_vec == outcome_positive,
                   exposure_vec == exposure_positive,
                   col_names,
                   conf_level)

  res <- structure(c(res, study_type),
                   class = "epi_2by2")

  return(res)
}


#' Calculate statistoics for 2 x 2 table
#'
#'
#' Given two logical vectors calculate relevant 2 x 2 statistocs
#'
#' @param outcome Logical vector for outcome.
#' @param exposure Logical vector for exposure
#' @param col_names Character vector, length 2, to name outcome and exposure
#' @param conf_level Probability for confidence interval calculations
#' @return Returns an epi_2by2 object.
#' @examples
#' head(mtcars)
#' epi_2by2(mtcars, hp>100, am==1)

calc_2by2 <- function(outcome, exposure, col_names, conf_level){

  tbl <- t(table(outcome,
                 exposure,
                 dnn = col_names))

  suppressWarnings(chisq <- chisq.test(tbl))

  prev <- binom.test(sum(outcome, na.rm = TRUE),
                     sum(!is.na(outcome)),
                     conf.level = conf_level)

  return(list(table = tbl,
              expected = chisq[["expected"]],
              chisq_pvalue = chisq[["p.value"]],
              prev_all = prev[["estimate"]][[1]],
              prev_all_ci = prev[["conf.int"]]))

}


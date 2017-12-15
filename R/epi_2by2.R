#' Two by two table analysis
#'
#' A technology test rather than a final product
#'
#' Given two columns of a dataframe (and 'positive' levels) generate a 2 x 2
#' table and appropriate summary statsitics.
#'
#' @param df A dataframe
#' @param outcome The unquoted column name for outcome.
#' @param exposure The unquoted column name for exposure
#' @param study_type The study type "cross", "cohort" or "case-control"
#' @param outcome_positive The "level" for a positive / of interest outcome
#' @param outcome_negative The "level" for a positive / of interest exposure
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

  tbl <- t(table(outcome_vec,
               exposure_vec,
               dnn = col_names))

  suppressWarnings(chisq <- chisq.test(tbl))

  prev <- binom.test(sum(outcome_vec == outcome_positive, na.rm = TRUE),
                     sum(!is.na(outcome_vec)),
                     conf.level = conf_level)

  return(structure(list(study_type = study_type,
                        table = tbl,
                        expected = chisq[["expected"]],
                        chisq_pvalue = chisq[["p.value"]],
                        prev_all = prev[["estimate"]][[1]],
                        prev_all_ci = prev[["conf.int"]]),
                   class = "epi_2by2"))
}




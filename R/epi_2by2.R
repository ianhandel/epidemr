#' Two by two table analysis
#'
#' A technology test rather than a final product
#'
#' Introduce S3 class epi and explore printing and plotting methods
#'
#' @param df A dataframe
#' @param outcome The unquoted column name for outcome.
#' @param exposure The unquoted column name for exposure
#' @return Returns an epi_2by2 object.
#' @examples
#' epi_2by2(mtcars, gear, carb)

# TODO for output - inspired by EpiTools from Ausvet
# 2 x 2 table!
# expected values 2 x 2
# Overall Incidence/Prevalence
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



epi_2by2 <- function(df, outcome, exposure){

  outcome <- rlang::enquo(outcome)
  exposure <- rlang::enquo(exposure)

  df <- dplyr::mutate(df, ..outcome = !!outcome)
  df <- dplyr::mutate(df, ..exposure = !!exposure)

  assertthat::assert_that(length(unique(df[["..outcome"]])) == 2)
  assertthat::assert_that(length(unique(df[["..exposure"]])) == 2)

  col_names <- c(rlang::quo_text(outcome), rlang::quo_text(exposure))

  tbl <- table(df[["..outcome"]],
               df[["..exposure"]],
               dnn = col_names)

  return(structure(list(table = tbl),
                   class = "epi_2by2"))
}




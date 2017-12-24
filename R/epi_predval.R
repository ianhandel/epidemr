#' Generate ppv and npv from test evaluation and prevalence range
#'

#'
#' @param x An epiR::epi.tests object or test sensitivity
#' @param sp test specicifity if x is sensitivity
#' @param prevalences vector of prevalences to calculate npv and ppv at
#' @return Returns an epi_predval object
#' @export
#' @examples
#' head(mtcars)
#' res <- epi_tests(mtcars, mpg < 25, cyl > 4, conf.level = 0.95)
#' epi_predval(res)

epi_predval <-function(X, Y = NULL,
                       prevalences = seq(0, 1, 0.05)){

  if(class(X) == "epi.tests"){
    se <- X$rval$se$est
    sp <- X$rval$sp$est
  }else{
    se  <- X
    sp <- Y
  }
  p <- prevalences

  ppv <- (se * p) / ((se * p) + ((1 - sp) * (1 - p)))
  npv <- sp * (1 - p) / ((sp * (1 - p) + (1 - se) * p))

  structure(tibble::tibble(se, sp, prev = p, ppv, npv),
            class = c("epi_predval", class(tibble::tibble())))
}

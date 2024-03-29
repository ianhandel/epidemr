#' Generate ppv and npv from test evaluation and prevalence range
#'

#'
#' @param X An epiR::epi.tests object or test sensitivity
#' @param Y Test specificity if not explicitly assigned to sp
#' @param se optionally test sensitivity
#' @param sp optionally test specificituy
#' @param prevalences vector of prevalences to calculate npv and ppv at
#' @return Returns an epi_predval object
#' @export
#' @examples
#' head(mtcars)
#' res <- epi_tests(mtcars, mpg < 25, cyl > 4, conf.level = 0.95)
#' epi_predval(res)

epi_predval <- function(X = NULL, Y = NULL, se = NULL, sp = NULL,
                        prevalences = seq(0, 1, 0.01)) {
  if (class(X) == "epi.tests") {
    se <- X$detail$se$est
    sp <- X$detail$sp$est
  } else {
    if (!is.null(se) & !is.null(sp)) {
      se <- se
      sp <- sp
    } else {
      se <- X
      sp <- Y
    }
  }
  p <- prevalences

  ppv <- (se * p) / ( (se * p) + ( (1 - sp) * (1 - p)))
  npv <- sp * (1 - p) / ( (sp * (1 - p) + (1 - se) * p))

  structure(
    tibble::tibble(se, sp, prev = p, ppv, npv),
    class = c("epi_predval", class(tibble::tibble()))
  )
}



#' Default plotting of a epi_predval object
#'
#' @param x An epi_predval object to plot
#' @param y ignored
#' @param ... ignored
#' @return ggplot object
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_line
#' @export
#' @examples
#' pv <- epi_predval(se = 0.90, sp = 0.99)
#' plot(pv)



plot.epi_predval <- function(x, y = NULL, ...) {
  ggplot(x, aes_(x = ~prev)) +
    geom_line(aes_(y = ~ppv, colour = "positive")) +
    geom_line(aes_(y = ~npv, colour = "negative")) +
    labs(
      x = "Prevalence",
      y = "Predicitive value",
      colour = "Predicitive \nvalue"
    )
}

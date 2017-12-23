library(tidyverse)
devtools::install_github("ianhandel/epidemr")
library(epidemr)

table(mtcars$am == 1, mtcars$cyl ==4, dnn = c("am==1", "cyl==4"))

epi_tests(mtcars, am == 1, cyl == 4)

epi_test(twobytwo(8, 3, 5, 16))


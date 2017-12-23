library(epidemr)
library(tidyverse)

(a <- epi_tests(c(1,2,3,4)))

b <- a$elements %>%
  discard(is.function) %>%
  flatten_dfr()

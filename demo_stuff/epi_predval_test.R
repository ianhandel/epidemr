library(epidemr)

epi_predval(0.90, 0.99)

epi_predval(se = 0.90,  sp =0.99)

epi_predval(epi_tests(c(1,2,3,4)))

epi_predval(epi_tests(c(1,2,3,4)), prevalences = seq(0,1,0.2))


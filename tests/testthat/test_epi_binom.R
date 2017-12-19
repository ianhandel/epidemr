# epi_binom tests

library(epidemr)
context("Simple binom cases")

dat <- data.frame(a = sample(1:100),
                  b = sample(rep(1:2, times = c(50,50))),
                  c = sample(rep(1:2, times = c(20, 80))),
                  d = rep(1, 100),
                  e = rep(0, 100))

# No Bayes  - as not MLE estimator
methods <- c("agresti-coull", "asymptotic", "cloglog", "exact",
             "logit", "probit", "profile", "lrt", "prop.test",
             "wilson")

test_that("estimates make sens", {
  expect_equal(all(epi_binom(dat, a<=50, methods = methods)[["proportion"]] == 0.50), TRUE)
  expect_equal(all(epi_binom(dat, b==1,  methods = methods)[["proportion"]]  == 0.50), TRUE)
  expect_equal(all(epi_binom(dat, c==2,  methods = methods)[["proportion"]]  == 0.80), TRUE)
  expect_equal(all(epi_binom(dat, d==1,  methods = methods)[["proportion"]]  == 1.0), TRUE)
  expect_equal(all(epi_binom(dat, e==1,  methods = methods)[["proportion"]]  == 0.0), TRUE)
})

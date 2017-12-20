# epi_2by2 tests

library(epidemr)
context("Simple table tests")



test_that("function tabulates", {
  expect_equivalent(
    epi_2by2(mtcars, outcome = am == 1, exposure = cyl == 4)$table,
    table(mtcars$cyl == 4, mtcars$am == 1)
  )
})

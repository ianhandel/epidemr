# epi_2by2 tests

library(epidemr)
context("Simple tests for tests")

df <- data.frame(
  gold = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 1),
  test = c(0, 1, 0, 0, 0, 0, 1, 0, 1, 1)
)
with(df, table(test, gold))

res <- epi_tests(df, gold == 1, test == 1)$tab

test_that("returns correct table", {
  expect_equal(trimws(res[1, 1]), I("3"))
  expect_equal(trimws(res[1, 2]), I("1"))
  expect_equal(trimws(res[2, 1]), I("0"))
  expect_equal(trimws(res[2, 2]), I("6"))
})

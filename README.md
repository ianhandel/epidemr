[![Build Status](https://travis-ci.org/ianhandel/epidemr.svg?branch=master)](https://travis-ci.org/ianhandel/epidemr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ianhandel/epidemr?branch=master&svg=true)](https://ci.appveyor.com/project/ianhandel/epidemr)
[![Coverage Status](https://img.shields.io/codecov/c/github/ianhandel/epidemr/master.svg)](https://codecov.io/github/ianhandel/epidemr?branch=master)

# epidemr
Simple, sometimes tidy, wrappers and helpers for epidemiology with R

It doesn't do anything clever - just wraps existing epidemiology functions (from epiR, binom and epitools packages) so they can be called with data frames and unquoted expressions.

It also provides 'tidying' functions to convert output to dataframes and provides simple plotting methods.

## Currently in development - so don't use it!

## But functions in draft so far...

(Although epi_tests and epi_2by2 may be removed and this package just provide epi_tables helper for epiR functions)

* __epi_tests__ - evaluation of diagnostic test vs a gold standard (also adds plot(...) method)
* __epi_binom__ - estimate proportions with binomial confidence intervals 
* __epi_predval__ - calculate positive and negative predictive values (also adds plot(...) method)
* __epi_table__ - make an outcome / exposure 2x2 table from dataframe and expressions
* __epi_2by2__ - preliminary wrapper for epi.2by2 that takes dataframe and expressions, or tables

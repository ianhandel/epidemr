[![Build Status](https://travis-ci.org/ianhandel/epidemr.svg?branch=master)](https://travis-ci.org/ianhandel/epidemr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ianhandel/epidemr?branch=master&svg=true)](https://ci.appveyor.com/project/ianhandel/epidemr)
[![Coverage Status](https://img.shields.io/codecov/c/github/ianhandel/epidemr/master.svg)](https://codecov.io/github/ianhandel/epidemr?branch=master)

# epidemr
A simple, tidy epidemiology package for R

It doesn't do anything clever - just wraps existing epidemiology functions (from epiR, binom and epitools packages) so they can be called with data frames and unquoted expressions.

It also provides 'tidying' functions to convert output to dataframes and provides simple plotting methods.

## Functions so far...
* __epi_tests__ - evaluation of diagnostic test vs a gold standard
* __epi_binom__ - estimate proportions with binomial confidence intervals 

[![Build Status](https://img.shields.io/travis/wamserma/R-wmisc/master.png)](https://travis-ci.org/wamserma/R-wmisc/) [![Windows Build Status](https://img.shields.io/appveyor/ci/wamserma/R-wmisc/master.svg)](https://ci.appveyor.com/project/wamserma/R-wmisc) [![Coverage Status](https://img.shields.io/codecov/c/github/wamserma/R-wmisc/master.svg)](https://codecov.io/github/wamserma/R-wmisc?branch=master)


# Wmisc - because everyone should have his own misc package for R

## Huh?

This is a collection of helper functions I wrote for myself. 

+ `Tokenizer`: A convenient method to read a file step-by-step with wrapper functions to read a file as a whole. For smaller files (a few dozen Megabytes) this is as fast as `base::readLines()` and `Kmisc::readlines()`. Quick benchmarks showed that for files of this size access times on mass storage (including caching by the OS) are magnitudes higher than actual read times. For big files the use case for `Tokenizer` is read-and-process/parse, reducing memory load compared to the other methods.

+ `Automat`: A tool to implement State Machines for transparent stateful computation. Idea sparked by [glyph/automat](https://github.com/glyph/automat). See the Vignette for details.

## I want it!

```r
install.packages(c("devtools")) # if you haven't done that before
devtools::install_github("wamserma/R-wmisc")
```

On Windows you will also need to install [RTools](https://cran.r-project.org/bin/windows/Rtools/).

## Building the Package manually

clone the git repository

```bash 
git clone https://github.com/wamserma/R-wmisc.git 
```

install the build dependencies

```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```

open project in RStudio and hit `Ctrl + Shift + D`, `Ctrl + Shift + B` 

It is suggested to use a GCC 5 or newer release to enable overflow checks in C, but the package will also build with the GCC 4.6 used in RTools 3.3.


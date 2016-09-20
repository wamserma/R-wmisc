[![Build Status](https://travis-ci.org/wamserma/R-wmisc.png)](https://travis-ci.org/wamserma/R-wmisc)

# WMisc - because everyone should have his own misc package for R

## Huh?

This is a collection of helper functions I wrote for myself. 


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


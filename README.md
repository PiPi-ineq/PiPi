# PiPi package

## Description

Calculate inequalities across social classes if the target variable can coded as 
a dummy variable (0, 1). To define the level of inequality the Lorenz Curve is 
used, which shows the distribution of participants (e.g. voters) across the 
pre-defined social classes (e.g. level of education). 

## Install

```r
if (requireNamespace("remotes", quietly = TRUE)) {
  remotes::install_gitlab("kogentum/pipi")
} else {
  stop("Install the 'remotes' package")
}
```

## Note

This is a work in progress - look for TODOs in the code base.

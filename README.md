
# prlm (Pseudo-observation Regularized LM)

<!-- badges: start -->
<!-- badges: end -->

Use `lm` in R adding while pseudo-observations to regularize every coefficient towards zero. `prlm` implements `lm` exactly except it adds an identity matrix to the `X` variables and a vector of `1`'s to the `y` variable.

See Bayesian Data Analysis Third Edition (Gelman et al.) Section 14.8 - "Including Numerical Prior Information" for method.

The insight here is that you can add a single observation to regularize a coefficient. This is weak in the sense of a single observation is not that much, and if a coefficient doesn't survive adding a single observation, you should be skeptical about it.

`prlm` does not try to get the Bayesian model right, or give you any knobs to turn. The goal is to just regularize every coefficient to `beta ~ N(0, sigma)` where `sigma` is the standard error of the regression and is implicit in this formulation.

## Installation

You can install the development version of prlm like so:

``` r
install_github("georgeberry/prlm")
```

## Example

``` r
library(prlm)

fit = lm(y ~ x)
fitreg = prlm(y ~ x)
```


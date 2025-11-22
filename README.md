
# prlm (Pseudo-observation Regularized LM)

<!-- badges: start -->
<!-- badges: end -->

Use `lm` in R while adding pseudo-observations to regularize every coefficient towards zero. `prlm` implements `lm` exactly except it adds an identity matrix to the `X` variables and a vector of `0`'s to the `y` variable, scaling the identity matrix to make regularization approximately equal to `beta ~ N(0, 1)`.

See Bayesian Data Analysis Third Edition (Gelman et al.) Section 14.8 - "Including Numerical Prior Information" for method.

The insight here is that you can add a single observation to regularize a coefficient. This is weak in the sense of a single observation is not that much, and if a coefficient doesn't survive adding a single observation, you should be skeptical about it.

This package is designed to add a bit of regularization to `lm` in a no-nonsense way, you should seek other solutions such as `brms`, STAN, Numpyro, etc. to fit more complex models.

## Installation

You can install the `prlm` like so:

``` r
devtools::install_github("georgeberry/prlm")
```

## Example

``` r
library(prlm)

fit = lm(y ~ x)
fitreg = prlm(y ~ x)  # this has coefs regularized to roughly beta ~ N(0,1)
```


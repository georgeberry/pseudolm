test_that("prlm matches manual augmented lm", {

  skip_on_cran()

  # Example data
  set.seed(1)
  df <- data.frame(
    x = rnorm(100),
    y = rnorm(100)
  )

  # Fit using your function
  fit1 <- prlm(y ~ x, df)

  # ---- Manually augment data exactly the same way ----
  mf <- model.frame(y ~ x, df)
  y  <- model.response(mf)
  X  <- model.matrix(attr(mf, "terms"), mf)

  p <- ncol(X)

  # identity rows & y=0
  X_aug <- rbind(X, diag(p))
  y_aug <- c(y, rep(0, p))

  # manual fit
  fit2 <- lm.fit(x = X_aug, y = y_aug)

  # ---- Compare results ----

  # 1. Coefficients
  expect_equal(
    as.numeric(fit1$coefficients),
    as.numeric(fit2$coefficients),
    tolerance = 1e-8
  )

  # 2. Fitted values
  expect_equal(
    as.numeric(fitted(fit1)[1:nrow(df)]),
    as.numeric(X %*% fit2$coefficients),
    tolerance = 1e-8
  )

  # 3. Residuals
  expect_equal(
    as.numeric(residuals(fit1)[1:nrow(df)]),
    as.numeric(y - X %*% fit2$coefficients),
    tolerance = 1e-8
  )
})


test_that("prlm1 matches manual augmented lm with sigma-scaled pseudo-obs", {

  skip_on_cran()

  # ---- Example data ----
  set.seed(1)
  df <- data.frame(
    x = rnorm(100),
    y = rnorm(100)
  )

  # Fit using prlm1
  fit1 <- prlm1(y ~ x, df)

  # ---- Reconstruct the pseudo-augmented system exactly ----

  # model frame + matrices
  mf <- model.frame(y ~ x, df)
  y  <- model.response(mf)
  X  <- model.matrix(attr(mf, "terms"), mf)
  p  <- ncol(X)

  # compute sigma exactly as prlm1 does
  prelim <- lm.fit(X, y)
  sigma  <- sqrt(sum(prelim$residuals^2) / prelim$df.residual)

  # construct sigma * I_p and pseudo y=0
  X_pseudo <- sigma * diag(p)
  y_pseudo <- rep(0, p)

  # augmented system
  X_aug <- rbind(X, X_pseudo)
  y_aug <- c(y, y_pseudo)

  # manual fit
  fit2 <- lm.fit(x = X_aug, y = y_aug)

  # ---- Compare results ----

  # 1. Coefficients should match
  expect_equal(
    as.numeric(fit1$coefficients),
    as.numeric(fit2$coefficients),
    tolerance = 1e-8
  )

  # 2. Fitted values on first n rows should match
  n <- nrow(df)
  expect_equal(
    as.numeric(fitted(fit1)[1:n]),
    as.numeric(X %*% fit2$coefficients),
    tolerance = 1e-8
  )

  # 3. Residuals on first n rows should match
  expect_equal(
    as.numeric(residuals(fit1)[1:n]),
    as.numeric(y - X %*% fit2$coefficients),
    tolerance = 1e-8
  )
})


test_that("prlm matches glmnet for ridge when glmnet is available", {

  skip_if_not_installed("MASS")

  library(MASS)

  set.seed(1)
  n <- 10000
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  y <- 2 * rowSums(X) + 0.5 * rnorm(n)

  df <- as.data.frame(cbind(y, X))
  colnames(df) <- c("y", paste0("x", 1:p))
  
  ols <- lm.fit(X, y)
  sigma <- sqrt(sum(ols$residuals^2) / ols$df.residual)


  # choose lambda

  # glmnet ridge (alpha = 0)
  fit_mass <- lm.ridge(y ~ X + 0, lambda = 1)

  # your fit
  fit_my  <- prlm1(y ~ . + 0, df)   # +0 = no intercept

  # 3. Residuals on first n rows should match
  expect_equal(
    as.numeric(coef(fit_my)),
    as.numeric(coef(fit_mass)),
    tolerance = 1e-3
  )

})

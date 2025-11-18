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

  # identity rows & y=1
  X_aug <- rbind(X, diag(p))
  y_aug <- c(y, rep(1, p))

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

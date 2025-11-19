#' Linear Model With Identity-Matrix Pseudo-Observations
#'
#' `prlm()` behaves exactly like [stats::lm()] but augments the regression
#' system with \eqn{p} pseudo-observations to regularize the coefficients to
#' roughly \eqn{\beta \sim \mathcal{N}(0, \sigma)} where \eqn{\sigma} is the
#' standard error of the regression.
#' 
#' Specifically, the model is fit to an augmented system
#'
#' \deqn{
#'   \tilde{X} = \begin{bmatrix} X \\ I_p \end{bmatrix}, \quad
#'   \tilde{y} = \begin{bmatrix} y \\ \mathbf{0}_p \end{bmatrix}
#' }
#'
#' This is equivalent to adding one pseudo-observation for each coefficient,
#' using the identity matrix as the design rows and a vector of ones as the
#' pseudo-response. The effect is a simple regularization that shrinks
#' coefficients toward 0.
#'
#' All other behavior—including handling of formulas, contrasts, weights,
#' NA processing, and returned object structure—matches `lm()` exactly.
#'
#' @param formula A formula specifying the model to be fitted.
#' @param data An optional data frame, list, or environment containing the
#'   variables in the model. See [stats::lm()].
#' @param subset Optional expression specifying a subset of observations.
#' @param weights Optional numeric vector of weights for weighted least squares.
#' @param na.action Function that indicates what should happen when `NA`s are
#'   encountered. See [stats::lm()].
#' @param method Method to be used; only `"qr"` is supported, as in `lm()`.
#' @param model Logical; if `TRUE`, include the model frame in the returned object.
#' @param x Logical; if `TRUE`, return the model matrix.
#' @param y Logical; if `TRUE`, return the response vector.
#' @param qr Logical; if `TRUE`, return the QR decomposition.
#' @param singular.ok Logical; if `FALSE`, a singular fit causes an error.
#' @param contrasts Optional list specifying contrasts for factor variables.
#' @param offset Optional offset to be included in the model.
#' @param ... Additional arguments passed through to [stats::model.frame()],
#'   [stats::lm.fit()], or [stats::lm.wfit()].
#'
#' @return An object of class `"lm"`, identical in structure to the output of
#'   [stats::lm()], but fitted on an augmented system including the pseudo-
#'   observations.
#'
#' @seealso [stats::lm()], [stats::lm.fit()]
#' @importFrom stats model.response model.weights model.offset
#' @importFrom stats is.empty.model model.matrix lm.fit lm.wfit
#' @importFrom stats .getXlevels
#' @export
prlm <- function (formula, data, subset, weights, na.action, method = "qr",
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
    contrasts = NULL, offset, ...) 
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame") 
        return(mf)
    else if (method != "qr") 
        warning(gettextf("method = '%s' is not supported. Using 'qr'", 
            method), domain = NA)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) 
        stop("'weights' must be a numeric vector")
    offset <- model.offset(mf)
    mlm <- is.matrix(y)
    ny <- if (mlm) 
        nrow(y)
    else length(y)
    if (!is.null(offset)) {
        if (!mlm) 
            offset <- as.vector(offset)
        if (NROW(offset) != ny) 
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                NROW(offset), ny), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
            ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
            y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
            0) else ny)
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)

        ## --- BEGIN: pseudo obs ---
        p <- ncol(x)

        # Identity matrix as pseudo-X
        X_pseudo <- diag(p)

        # Vector of 0s as pseudo-y
        y_pseudo <- rep(0, p)

        # Combine augmented data
        x <- rbind(x, X_pseudo)
        y <- c(y, y_pseudo)

        # If weights were used, append equal weights
        if (!is.null(w)) {
            w <- c(w, rep(1, p))
        }
        ## --- END: pseudo obs ---

        z <- if (is.null(w)) 
            lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
            ...)
    }
    class(z) <- c(if (mlm) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- x
    if (ret.y) 
        z$y <- y
    if (!qr) 
        z$qr <- NULL
    z
}



#' LM with Pseudo-Observations to Regularize Betas to N(0,1)
#'
#' `prlm1()` behaves like [stats::lm()] but augments the regression
#' system with \eqn{p} pseudo-observations and scales these so that the prior
#' on each coefficient is roughly \eqn{\beta \sim \mathcal{N}(0, 1)}. It does
#' this by fitting [stats::lm()] on the data, getting the standard error of
#' the regression \eqn{\sigma} and then scaling the pseudo-observation identity
#' matrix.
#' 
#' This is equivalent to adding one pseudo-observation for each coefficient,
#' using the identity matrix as the design rows and a vector of ones as the
#' pseudo-response. The effect is a simple regularization that shrinks
#' coefficients toward 0.
#' 
#' \deqn{
#'   \tilde{X} = \begin{bmatrix} X \\ \sigma I_p \end{bmatrix}, \quad
#'   \tilde{y} = \begin{bmatrix} y \\ \mathbf{0}_p \end{bmatrix}
#' }
#'
#' All other behavior—including handling of formulas, contrasts, weights,
#' NA processing, and returned object structure—matches `lm()` exactly.
#'
#' @param formula A formula specifying the model to be fitted.
#' @param data An optional data frame, list, or environment containing the
#'   variables in the model. See [stats::lm()].
#' @param subset Optional expression specifying a subset of observations.
#' @param weights Optional numeric vector of weights for weighted least squares.
#' @param na.action Function that indicates what should happen when `NA`s are
#'   encountered. See [stats::lm()].
#' @param method Method to be used; only `"qr"` is supported, as in `lm()`.
#' @param model Logical; if `TRUE`, include the model frame in the returned object.
#' @param x Logical; if `TRUE`, return the model matrix.
#' @param y Logical; if `TRUE`, return the response vector.
#' @param qr Logical; if `TRUE`, return the QR decomposition.
#' @param singular.ok Logical; if `FALSE`, a singular fit causes an error.
#' @param contrasts Optional list specifying contrasts for factor variables.
#' @param offset Optional offset to be included in the model.
#' @param ... Additional arguments passed through to [stats::model.frame()],
#'   [stats::lm.fit()], or [stats::lm.wfit()].
#'
#' @return An object of class `"lm"`, identical in structure to the output of
#'   [stats::lm()], but fitted on an augmented system including the pseudo-
#'   observations.
#'
#' @seealso [stats::lm()], [stats::lm.fit()]
#' @importFrom stats model.response model.weights model.offset
#' @importFrom stats is.empty.model model.matrix lm.fit lm.wfit
#' @importFrom stats .getXlevels
#' @export
prlm1 <- function (formula, data, subset, weights, na.action, method = "qr",
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
    contrasts = NULL, offset, ...) 
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame") 
        return(mf)
    else if (method != "qr") 
        warning(gettextf("method = '%s' is not supported. Using 'qr'", 
            method), domain = NA)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) 
        stop("'weights' must be a numeric vector")
    offset <- model.offset(mf)
    mlm <- is.matrix(y)
    ny <- if (mlm) 
        nrow(y)
    else length(y)
    if (!is.null(offset)) {
        if (!mlm) 
            offset <- as.vector(offset)
        if (NROW(offset) != ny) 
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                NROW(offset), ny), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
            ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
            y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
            0) else ny)
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)

        ## --- BEGIN: pseudo obs ---
        
        # get sigma

        fit <- lm.fit(x, y)
        sigma <- sqrt(sum(fit$residuals^2) / fit$df.residual)
        
        p <- ncol(x)

        # Identity matrix as pseudo-X
        X_pseudo <- sigma * diag(p)

        # Vector of 0s as pseudo-y
        y_pseudo <- rep(0, p)

        # Combine augmented data
        x <- rbind(x, X_pseudo)
        y <- c(y, y_pseudo)

        # If weights were used, append equal weights
        if (!is.null(w)) {
            w <- c(w, rep(1, p))
        }
        ## --- END: pseudo obs ---

        z <- if (is.null(w)) 
            lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
            ...)
    }
    class(z) <- c(if (mlm) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- x
    if (ret.y) 
        z$y <- y
    if (!qr) 
        z$qr <- NULL
    z
}

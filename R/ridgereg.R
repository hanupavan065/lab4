
#' Fit a Ridge Regression Model
#'
#' Fits ridge regression using standardized predictors and QR decomposition.
#'
#' @param formula A model formula.
#' @param data A data frame containing the model variables.
#' @param lambda A non-negative ridge penalty.
#' @return An object of class ridgereg.
#' @export
ridgereg <- function(formula, data, lambda = 1) {

  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!inherits(formula, "formula") || length(formula) != 3)
    stop("formula must be a two-sided formula")
  if (length(lambda) != 1 || !is.numeric(lambda) ||
      !is.finite(lambda) || lambda < 0)
    stop("lambda must be a finite, non-negative number")

  mf <- stats::model.frame(
    formula, data = data, na.action = stats::na.fail
  )
  y <- stats::model.response(mf)

  if (!is.numeric(y) || !is.null(dim(y)))
    stop("response must be numeric")

  X <- stats::model.matrix(formula, data = mf)

  if (any(!is.finite(X)) || any(!is.finite(y)))
    stop("data must contain finite values")

  intercept <- which(colnames(X) == "(Intercept)")
  predictors <- setdiff(seq_len(ncol(X)), intercept)

  centers <- rep(0, ncol(X))
  scales <- rep(1, ncol(X))

  if (length(predictors)) {
    centers[predictors] <- colMeans(X[, predictors, drop = FALSE])
    scales[predictors] <- apply(
      X[, predictors, drop = FALSE],
      2,
      stats::sd
    )

    if (any(!is.finite(scales[predictors])) ||
        any(scales[predictors] == 0))
      stop("predictors must have non-zero variance")

    X[, predictors] <- scale(
      X[, predictors, drop = FALSE],
      center = centers[predictors],
      scale = scales[predictors]
    )
  }

  # Do not penalize the intercept
  D <- diag(ncol(X))
  if (length(intercept)) D[intercept, intercept] <- 0

  # QR-based ridge regression (starred exercise)
  A <- rbind(X, sqrt(lambda) * D)
  b <- c(y, rep(0, ncol(X)))
  beta <- qr.solve(A, b)

  names(beta) <- colnames(X)

  result <- list(
    coefficients = beta,
    fitted.values = as.vector(X %*% beta),
    residuals = as.vector(y - X %*% beta),
    formula = formula,
    terms = stats::terms(formula),
    xlevels = stats::.getXlevels(stats::terms(formula), mf),
    contrasts = attr(X, "contrasts"),
    lambda = lambda,
    centers = centers,
    scales = scales,
    predictors = predictors
  )

  class(result) <- "ridgereg"
  result
}


#' Print a Ridge Regression Model
#' @param x A ridgereg model.
#' @param ... Additional arguments.
#' @export
print.ridgereg <- function(x, ...) {
  cat("Ridge regression\nLambda:", x$lambda, "\n")
  print(x$coefficients)
  invisible(x)
}


#' Extract Ridge Regression Coefficients
#' @param object A ridgereg model.
#' @param ... Additional arguments.
#' @export
coef.ridgereg <- function(object, ...) {
  object$coefficients
}


#' Predict from a Ridge Regression Model
#' @param object A ridgereg model.
#' @param newdata Optional data frame for prediction.
#' @param ... Additional arguments.
#' @return Numeric predictions.
#' @export
predict.ridgereg <- function(object, newdata, ...) {

  if (missing(newdata)) return(object$fitted.values)

  tt <- stats::delete.response(object$terms)

  mf <- stats::model.frame(
    tt, data = newdata,
    xlev = object$xlevels,
    na.action = stats::na.pass
  )

  Xnew <- stats::model.matrix(
    tt, data = mf,
    contrasts.arg = object$contrasts
  )

  j <- object$predictors

  if (length(j)) {
    Xnew[, j] <- sweep(
      Xnew[, j, drop = FALSE], 2,
      object$centers[j], "-"
    )
    Xnew[, j] <- sweep(
      Xnew[, j, drop = FALSE], 2,
      object$scales[j], "/"
    )
  }

  if (!identical(colnames(Xnew), names(object$coefficients)))
    stop("newdata has incompatible predictor columns")

  as.vector(Xnew %*% object$coefficients)
}

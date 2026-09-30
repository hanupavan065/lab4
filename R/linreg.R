#'linear regression using QR
#' @param formula A model formula
#' @param data A data frame containing the model variables.
#' @return An object of class linreg containing the regression results.
#' @export
linreg<-function(formula,data) {
  if(!is.data.frame(data)){
    stop("invalid")
  }
  if (!inherits(formula,"formula") || length(formula) != 3) {
    stop("invalid")
  }
  var_names<-setdiff(all.vars(formula), ".")

  missing_names <- setdiff(var_names, names(data))

  if (length(missing_names) > 0L) {
    stop(
      "Variables missing from data: ",
      paste(missing_names, collapse = ", ")
    )
  }
  model_data<-stats::model.frame(
    formula,
    data=data,
    na.action=stats::na.fail,
    drop.unused.levels=TRUE
  )
  if (!is.null(stats::model.offset(model_data))){
    stop("invalid")
  }
  X <- stats::model.matrix(formula, data = model_data)
  y <- stats::model.response(model_data)

  if (!is.numeric(y) | !is.null(dim(y))) {
    stop("invalid")
  }

  if (any(!is.finite(X)) | any(!is.finite(y))) {
    stop("invalid.")
  }

  # Number of observations and coefficients
  n<-nrow(X)
  p<-ncol(X)

  if(p==0){
    stop("invalid")
  }
  if (n<=p) {
    stop("invalid")
  }

  # QR decomposition
  qr_X<-qr(X)

  if(qr_X$rank<p) {
    stop("invalid")
  }

  R<-qr.R(qr_X)

  # Solve for coefficients in QR column order
  b_pivot<-backsolve(
    R,
    qr.qty(qr_X, y)[seq_len(p)]
  )

  # Restore the original predictor order
  b_hat<-numeric(p)
  b_hat[qr_X$pivot]<-b_pivot
  names(b_hat)<-colnames(X)
  fitted_values<-as.vector(X %*% b_hat)
  res<-as.vector(y - fitted_values)

  names(fitted_values)<-rownames(model_data)
  names(res)<-rownames(model_data)

  # Degrees of freedom and residual variance
  d<-n-p
  alpha2<-sum(res^2)/d
  alpha<-sqrt(alpha2)

  # Coefficient covariance using QR
  R_inverse<-backsolve(R,diag(p))
  cov_pivot <- alpha2 * tcrossprod(R_inverse)

  real_order<-order(qr_X$pivot)

  var_b<-cov_pivot[
    real_order,
    real_order,
    drop = FALSE
  ]

  dimnames(var_b)<-list(colnames(X),colnames(X))

  # Standard errors, t-values, and two-sided p-values
  standard_error<-sqrt(diag(var_b))
  t_values<-b_hat/standard_error

  p_values<-2*stats::pt(
    abs(t_values),
    df=d,
    lower.tail = FALSE
  )

  # Leverage and standardized residuals for diagnostic plots
  lev<-rowSums(qr.Q(qr_X)^2)

  denominator<-alpha * sqrt(pmax(0,1-lev))
  standardized_res<-res/denominator

  standardized_res[
    denominator<=0 | !is.finite(standardized_res)
  ] <- NA_real_

  # Store the results
  result<-list(
    call=match.call(),
    formula=formula,
    coeff=b_hat,
    fitted_values=fitted_values,
    res=res,
    d=d,
    alpha2=alpha2,
    alpha=alpha,
    var_b=var_b,
    standard_error=standard_error,
    t_values=t_values,
    p_values=p_values,
    lev=lev,
    standardized_res= standardized_res
  )

  class(result)<-"linreg"

  return(result)
}
residuals.linreg<-function(object,...) {
  return(object$res)
}



#'Print a linreg model
#' @param x linreg object.
#' @param ... extra arguments.
#' @return A vector of regression coefficients
#' @export
print.linreg<-function(x,...){
  print(x$call)
  print(x$coeff)
}


#'coefficients
#' @param object A linreg object.
#' @param ... extra arguments.
#' @return vector of regression coefficients.
#' @export
coef.linreg<-function(object, ...) {
  return(object$coeff)
}


#'residuals
#' @param object A linreg object.
#' @param ... extra arguments.
#' @return vector of residuals.
#' @export
resid.linreg<-function(object, ...) {
  return(object$res)
}


#' values
#' @param object A model object.
#' @param ... extra  arguments.
#' @return A numeric vector of predicted values.
#' @export
pred<-function(object, ...) {
  UseMethod("pred")
}


#'fitted values
#' @param object A linreg object.
#' @param ... Additional arguments.
#' @return A numeric vector of fitted values.
#' @export
pred.linreg<-function(object, ...) {
  return(object$fitted_values)
}


#' Summary of a linear model
#' @param object linreg object.
#' @param ... extra arguments.
#' @return Prints the model summary.
#' @export
summary.linreg<-function(object, ...) {
  tab<-cbind(
    Estimate=object$coeff,
    "Std. Error"=object$standard_error,
    "t value"=object$t_values,
    "Pr(>|t|)"=object$p_values
  )

  stats::printCoefmat(tab,digits=4,signif.stars=TRUE)

  cat(
    "\nResidual standard error:", format(object$alpha, digits = 4),
    "on", object$d, "degrees of freedom\n"
  )
}


#' Plot a linreg model
#' @param x linreg object
#' @param ... extra arguments.
#' @return plot
#' @importFrom rlang .data
#' @export
plot.linreg<-function(x, ...) {
  plot_data<-data.frame(
    f=x$fitted_values,
    r=x$res,
    scale_loc=sqrt(abs(x$standardized_res))
  )

  plot1<-ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x=.data$f, y=.data$r)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=0, linetype="dashed") +
    ggplot2::labs(
      title="Residuals vs Fitted",
      x="Fitted values",
      y="Residuals"
    )

  plot2<-ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x=.data$f,y=.data$scale_loc)
  ) +
    ggplot2::geom_point(na.rm=TRUE) +
    ggplot2::labs(
      title="Scale-Location",
      x="Fitted values",
      y="sqrt(|Standardized residuals|)"
    )

  print(plot1)
  print(plot2)
}

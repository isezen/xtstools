#' BoxCox Transformation
#'
#' \code{boxcox} function applies boxcox-transformation to \code{x}. Calculated
#' best \code{lambda} value for the transformation is attached to \code{x} as
#' attribute. (See: \code{attr(x, "lambda")}). If \code{x} contains any value
#' equal or less than \code{0}, it will be set to \code{NA} for the sake of
#' transformation.
#'
#' @param x A numeric vector-like R object.
#' @param lambda If \code{lambda} is a single value, transformation is
#'               accomplished using this value. If a 2-length vector, best
#'               lambda value is calculated for transformation. \code{lambda}
#'               always must be 2-length vector for \code{boxcox.lambda}
#'               function.
#' @param inverse If \code{TRUE}, inverse boxcox transformation is calculated.
#' @export
boxcox <- function(x, lambda = c(-2, 2), inverse = FALSE) {
  inverse <- is.logical(inverse) && isTRUE(inverse)
  xn <- as.numeric(x); non_na <- !is.na(x)
  xn <- xn[non_na]
  if (inverse) {
    att_lambda <- attr(x, "lambda")
    lambda <- ifelse(is.null(att_lambda), lambda[1], att_lambda)
  } else {
    if (any(xn <= 0)) {
      warning(paste0("Any zero or negative value (which x contains) will be set",
                     " to NA for the sake of transformation"))
      xn[xn <= 0] <- NA
    }
    if (length(lambda) > 1)
      lambda <- boxcox.lambda(xn, lambda)
  }
  x[non_na] <- boxcox_internal(xn[non_na], lambda, inverse)
  attr(x, "lambda") <- if (inverse) NULL else lambda
  x
}

#' @description \code{boxcox.lambda} function calculates best lambda value for
#'              boxcox transformation in the interval of \code{lambda} values.
#' @rdname boxcox
#' @export
boxcox.lambda <- function(x, lambda = c(-2, 2)) {
  if (length(lambda) == 1) lambda <- c(-abs(lambda), abs(lambda))
  lambda <- range(lambda)
  x <- as.vector(x); x <- x[!is.na(x)]
  if (any(x <= 0)) {
    warning(paste0("Any zero or negative value (which x contains) will be set",
                   " to NA for the sake of transformation"))
    x[x <= 0] <- NA
  }
  stats::optimize(function(i) loglik(x[!is.na(x)], i), lambda,
                  maximum = TRUE, tol = sqrt(.Machine$double.eps))$maximum
}

#' @description \code{boxcox.plus} calculates a value to add \code{x} values
#'              which maximizes \code{loglik} function for transformation.
#' @param plus Interval to be searched for adittive value.
#' @rdname boxcox
#' @export
boxcox.plus <- function(x, plus = c(0, 100)) {
  if (length(plus) > 2) plus <- range(plus)
  if (length(plus) == 1) {
    if (plus < 0) plus <- -plus
    plus <- c(0, plus)
  }
  x <- as.vector(x); x <- x[!is.na(x)]
  if (any(x <= 0)) plus <- plus + abs(min(x))
  op <- stats::optimize(function(i) loglik(x + i, 0), plus,
                        maximum = TRUE, tol = sqrt(.Machine$double.eps))
  op$maximum
}

#
loglik <- function(x, lambda = 0) {
  n <- length(x); t <- boxcox_internal(x, lambda)
  - (n/2) * log(sum((t - mean(t))^2)/n) + (lambda - 1) * sum(log(x))
}

boxcox_internal <- function(x, lambda, inverse = FALSE) {
  if (inverse) {
    if (lambda == 0) exp(x) else ((x * lambda) + 1)^(1/lambda)
  } else {
    if (lambda == 0) log(x) else (x^lambda - 1)/lambda
  }
}

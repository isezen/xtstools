#' dlambda <- function(x, lambda = 0) {
#'   if (length(lambda) > 1) {
#'     dl <- sapply(lambda, function(i) dlambda(x, i))
#'     names(dl) <- lambda
#'     return(dl)
#'   }
#'   t <- VGAM::yeo.johnson(x, lambda)
#'   abs(mean(t) - median(t))/IQR(t)
#' }
#'
#' #' YEO Transformation
#' #'
#' #' @export
#' yeo <- function(x, lambda = c(-1, 1), inverse = FALSE) {
#'   inverse <- isTRUE(inverse)
#'   xn <- as.numeric(x); non_na <- !is.na(x)
#'   if (inverse) {
#'     lam <- attr(x, "lambda")
#'     lambda <- if (is.null(lam)) lambda[1] else lam
#'   } else {
#'     if (length(lambda) > 1) {
#'       dl <- optimize(function(i) dlambda(xn[non_na], i), range(lambda))
#'       lambda <- dl$minimum
#'     }
#'   }
#'   x[non_na] <- VGAM::yeo.johnson(xn[non_na], lambda, inverse = inverse)
#'   if (!inverse) attr(x, "lambda") <- lambda
#'   return(x)
#' }


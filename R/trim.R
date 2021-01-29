#' Trim data
#'
#' Trim the data by sorting from each end. If \code{x} has dimension more than
#' one, each column trimmed sperately.
#'
#' @param x An R object.
#' @param trim Fraction of observations to be trimmed from each end of \code{x}.
#'             Number of trimmed values are calculated on Non-NA values.
#' @param na.rm A logical value indicating whether trimmed values should be
#'             stripped or not.
#' hour, mday, mon, year, wday, yday, isdst} and \code{yearmon, wdayhour, md,
#' mdh, yhour, index}.
#'
#' @export
trim <- function(x, trim, na.rm) UseMethod("trim")

#' @describeIn trim default S3 method
#' @export
trim.default <- function(x, trim = 0.15, na.rm = FALSE) {
  if (NCOL(x) == 1) {
    i <- order(x)
    i2 <- i[!is.na(x[i])]
    n <- length(i2)
    lo <- floor(n * trim)
    hi <- n + 1 - lo
    j <- i2[c(1:lo, hi:n)]
    if (na.rm)
      x <- x[-j]
    else
      x[j] <- NA
  } else {
    x <- sapply(x, trim.default, trim = trim, na.rm = na.rm)
  }
  return(x)
}

#' @describeIn trim S3 method for class \code{zoo}
#' @export
trim.zoo <- function(x, ...) {
  as_list <- utils::getFromNamespace("as.list.xts", "xts")
  do.call(cbind, parallel::mclapply(as_list(x), trim.default))
}

#' Mode of Numeric vector
#'
#' If \code{x} is a double-precision numeric vector, function calculates peak
#' (maximum) of Kernel Density Estimation of \code{x} values. If \code{x} is
#' integer or another R object, it's calcualted highest number of occurrences
#' in \code{x}.
#'
#' @param x A vector-like R object.
#' @export
mode2 <- function(x) {
  modef <- function(x, n = 9, from = min(x, na.rm = TRUE),
                   to = max(x, na.rm = TRUE)) {
    d <- density(x, n = 2^n, na.rm = TRUE, from = from, to = to)
    imax <- which.max(d$y)
    if (imax == 1 | imax == 2^n) {
      d$x[imax]
    } else {
      modef(x, n + 1, d$x[imax - 1], d$x[imax + 1])
    }
  }
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  if (is.numeric(x) && any(as.integer(x) != x))
    modef(x)
  else
    get_mode(x)
}

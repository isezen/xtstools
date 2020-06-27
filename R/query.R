#' Subset POSIXt/zoo/xts objects
#'
#' Return subsets of \code{POSIXt}, \code{zoo} and \code{xts} objects
#' which meet \code{query} condition.
#'
#' @param x A \code{POSIXt}, \code{zoo} and \code{xts} objects
#' @param query Condition to query object
#' @param which.i If \code{TRUE}, indices are returned
#' @param ... Unused
#' @examples
#' library(xts)
#' data(sample_matrix)
#' x <- as.xts(sample_matrix, dateFormat="Date")
#' query(x, "month == 10")
#' @seealso \link[base]{subset}
#'
#' @export
query <- function(x, query, which.i, ...) UseMethod("query")

#' @describeIn query S3 method for class \code{POSIXt}
#' @export
query.POSIXt <- function(x, query, which.i = FALSE, ...) {
  r <- if (missing(query)) {
    rep_len(TRUE, length(x))
  } else {
    year <- indexx(x, "year")
    month <- indexx(x, "mon")
    day <- indexx(x, "mday")
    hour <- indexx(x, "hour")
    e <- substitute(query)
    e <- if (is.character(query)) eval(parse(text = query)) else eval(query)
    r <- eval(e, envir = environment())
    if (!is.logical(r))
      stop("'query' result must be logical")
    r & !is.na(r)
  }
  if (which.i) r else x[r]
}

#' @describeIn query S3 method for class \code{zoo}
#' @export
query.zoo <- function(x, query, which.i = FALSE, ...) {
  i <- if (inherits(x, "xts")) index.xts(x) else index.zoo(x)
  i <- query.POSIXt(i, query, which.i = TRUE)
  if (!which.i) {
    if (!is.null(dim(x)) && ncol(x) > 1) x[i,] else x[i]
  } else i
}

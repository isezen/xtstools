calc_indices <- function(len, step_size, wsize, clamped) {
  middle <- ceiling(len/2)
  by <- step_size[1]
  from <- middle - by * floor((middle - 1)/by)
  to <- middle + by * floor((len - middle)/by)
  steps <- seq(from, to, by)
  # steps <- c(rev(seq(middle, 1, -by)[-1]),
  #            seq(middle, len, by))
  i <- wseq(seq_len(len), steps, size = wsize[1],
            clamped = clamped)
  if (is.matrix(i)) {
    step_size <- step_size[-1]
    wsize <- wsize[-1]
    if (length(wsize) > 0) {
      i <- apply(i, 2, function(x) {
        x[calc_indices(length(x), step_size, wsize, clamped)]
      })
    }
  }
  i
}

#' Extended - Apply a Function over array Margins
#'
#' Returns a vector or array or list of values obtained by applying a function
#' to margins of an array or matrix.
#'
#' @param x An array, including matrix.
#' @param MARGIN A vector giving the subscripts which the function will be
#'               applied over. E.g., for a matrix 1 indicates rows, 2 indicates
#'               columns, c(1, 2) indicates rows and columns. Where X has named
#'               dimnames, it can be a character vector selecting dimension
#'               names.
#' @param FUN the function to be applied. See \code{\link{apply}}.
#' @param step_size Step size of moving window over margin. This value can be a
#'                  vector. Each value in the vector represents step size for a
#'                  new window in previous window.
#' @param wsize Window size for each \code{step_size}. This value can be a
#'              vector. Each value in the vector represents window size for a
#'              new window in previous window.
#' @param clamped This parameter defines hoe the windows will behave at the
#'                edges of margin. If \code{TRUE}, windows are extended through
#'                end and begining of the margins.
#' @param ... Other arguments passed to \code{FUN}.
#'
#' @export
wapply <- function(x, ...) UseMethod("wapply")

#' @describeIn wapply Default S3 method
#' @export
wapply.default <- function(x, MARGIN, FUN, ..., step_size = 1, wsize = 1,
                           clamped = TRUE) {
  if (step_size == 1 && wsize == 1) {
    apply(x, MARGIN, FUN, ...)
  } else {
    calc_indices <- utils::getFromNamespace("calc_indices", "xtstools")
    i <- calc_indices(dim(x)[MARGIN], step_size, wsize, clamped)
    fnc <- function(j) FUN(slice_array(x, MARGIN, j), ...)
    if (is.matrix(i)) apply(i, 2, fnc) else sapply(i, fnc)
  }
}

#' @describeIn wapply S3 method for zoo object
#' @param multiple This argument determines how \code{wapply} function will
#'                 behave If \code{x} is a \code{zoo} object has columns more
#'                 than one. If \code{multiple = FALSE} and \code{NCOL(x) > 1},
#'                 each column is estimated seperately. Default is \code{FALSE}.
#' @param mclapply If intented to use \code{\link[parallel]{mclapply}}, set
#'                 \code{TRUE}. Otherwise, \code{\link[parallel]{parLapply}} is
#'                 used. Default is \code{TRUE}.
#' @export
wapply.zoo <- function(x, FUN, ..., step_size = 1, wsize = 1, clamped = TRUE,
                       multiple = FALSE, mclapply = TRUE) {
  if (NCOL(x) > 1 && !multiple) {
    as_list <- utils::getFromNamespace("as.list.xts", "xts")
    lx <- as_list(x)
    if (mclapply) {
      lx <- parallel::mclapply(lx, wapply, FUN,
                               step_size = step_size,
                               wsize = wsize, clamped = clamped,
                               multiple = multiple, mclapply = mclapply, ...)
    } else {
      cl <- parallel::makeCluster(getOption("mc.cores", 2L),
                                  setup_timeout = 0.5)
      lx <- parallel::parLapply(cl, lx, wapply, FUN, step_size = step_size,
                                wsize = wsize, clamped = clamped,
                                multiple = multiple, mclapply = mclapply, ...)
      parallel::stopCluster(cl)
    }
    simplify2array(lx)
  } else {
    wapply(as.tsarray(x), MARGIN = 1, FUN = FUN, ...,
           step_size = step_size, wsize = wsize, clamped = clamped)
  }
}

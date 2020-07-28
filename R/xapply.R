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
#' @param An array, including matrix.
#' @param margin A vector giving the subscripts which the function will be
#'               applied over. E.g., for a matrix 1 indicates rows, 2 indicates
#'               columns, c(1, 2) indicates rows and columns. Where X has named
#'               dimnames, it can be a character vector selecting dimension
#'               names.
#' @param step_size Step size of moving window over margin. This value can be a
#'                  vector. Each value in the vector represents step size for a
#'                  new window in previous window.
#' @param wsize Window size for each \code{step_size}. This value can be a
#'              vector. Each value in the vector represents window size for a
#'              new window in previous window.
#' @param clamped This parameter defines hoe the windows will behave at the
#'                edges of margin. If \code{TRUE}, windows are extended through
#'                end and begining of the margins.
#' @param multiple This argument determines how \code{xapply} function will
#'                 behave If \code{x} is an array having a dimension equal to
#'                 3. If \code{multiple = FALSE} and dimension is 3, matrices in
#'                 each third dimension are estimated as seperate matrices.
#'                 Default is \code{FALSE}.
#' @param ... Other arguments passed to \code{FUN}.
#'
#' @export
xapply <- function(x, ...) UseMethod("xapply")

#' @describeIn xapply Default S3 method
#' @export
xapply.default <- function(x, MARGIN, FUN, ..., step_size = 1, wsize = 1,
                           clamped = TRUE, multiple = FALSE) {
  if (length(dim(x)) == 3 & !multiple) {
    nc <- getOption("mc.cores", 2L)
    if (nc > 1) {
      cl <- parallel::makeCluster(nc, setup_timeout = 0.5)
      parallel::clusterExport(cl, list("calc_indices"), envir = environment())
      ret <- parallel::parApply(cl, x, 3, xapply, MARGIN, FUN, ...,
                                step_size = step_size, wsize = wsize,
                                clamped = clamped, multiple = multiple)
      parallel::stopCluster(cl)
    } else {
      ret <- apply(x, 3, xapply, MARGIN, FUN, ...,
                   step_size = step_size, wsize = wsize,
                   clamped = clamped, multiple = multiple)
    }
    rownames(ret) <- rownames(x)
    ret
  } else {
    i <- calc_indices(dim(x)[MARGIN], step_size, wsize, clamped)
    if (is.matrix(i)) {
      apply(i, 2, function(j) FUN(slice_array(x, MARGIN, j), ...))
    } else {
      sapply(i, function(j) FUN(slice_array(x, MARGIN, j), ...))
    }
  }
}

#' @describeIn xapply S3 method for zoo object
#' @export
xapply.zoo <- function(x, FUN, ..., step_size = 1, wsize = 1, clamped = TRUE,
                       multiple = FALSE) {
  if (NCOL(x) > 1 && !multiple) {
    as_list <- utils::getFromNamespace("as.list.xts", "xts")
    lx <- as_list(x)
    lx <- parallel::mclapply(lx, xapply, FUN,
                             step_size = step_size,
                             wsize = wsize, clamped = clamped,
                             multiple = multiple, ...)
    simplify2array(lx)
  } else {
    xapply(as.tsarray(x), MARGIN = 1, FUN = FUN, ...,
           step_size = step_size, wsize = wsize, clamped = clamped,
           multiple = multiple)
  }
}

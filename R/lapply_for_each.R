#' Apply function for each same months in a zoo series
#'
#' @param x A \code{zoo} series
#' @param FUN a function to apply.
#' @param ... Other arguments passed to function.
#'
#' @name lapply_for_each
NULL

#' @rdname lapply_for_each
#' @export
lapply_for_each_month <- function(x, FUN, ...) {
  if (!inherits(x, "zoo")) stop("x must be a 'zoo' object")
  lapply(1:12, function(m) {
    FUN(xtstools::query(x, paste0("month == ", m)), ...)
  })
}

#' @rdname lapply_for_each
#' @export
lapply_for_each_part <- function(x, FUN, ..., npart = 52) {
  if (length(npart) != 1) stop("npart must be a single value")
  if (!inherits(x, "zoo")) stop("x must be a 'zoo' object")
  # if (NCOL(x) > 1) {
  #   as_list <- utils::getFromNamespace("as.list.xts", "xts")
  #   lx <- as_list(x)
  #   cl <- parallel::makeCluster(getOption("mc.cores", 2L), setup_timeout = 0.5)
  #   lx <- parallel::parLapply(cl, lx, lapply_for_each_part, ..., npart = npart)
  #   parallel::stopCluster(cl)
  # } else {
    # browser()
    # TODO: TEMPORARY SOLUTION --------
    y <- as.tsarray(x); n <- NROW(y)
    i <- if (npart <= 1) list(seq_len(n)) else
      split(seq_len(n), cut(seq_len(n), round(npart)))
    names(i) <- seq_len(length(i))
    tm <- attr(y, "tm")
    frm <- attr(y, "from")
    l <- lapply(i, function(i) y[i,])
    ltm <- lapply(i, function(i) tm[i,])
    a <- l[[1]]
    b <- ltm[[1]]
    l2 <- mapply(function(a, b) {
      attr(a, "tm") <- b
      index <- unlist(b)
      index <- index[!is.na(index)]
      attr(index, "tzone") <- attr(frm$index, "tzone")
      attr(index, "tclass") <- attr(frm$index, "tclass")
      attr(a, "from") <- list(index = index, class = frm$class,
                              dim = c(length(index), 1),
                              dimnames = frm$dimnames)
      attr(a, "class") <- "tsarray"
      xts::as.xts(a)
    }, l, ltm)
    # TODO: TEMPORARY SOLUTION --------
    lx <- lapply(l2, FUN, ...)
  # }
  return(lx)
}

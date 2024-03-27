#' Median Polish for xts time series
#'
#' Fits an additive model (twoway decomposition) using Tukey's median
#' polish procedure. If xts object contains data belong to February 29, it's
#' excluded before medpolish and included after medpolish.
#'
#' @param x A numeric matrix or xts object
#' @param ... Other params passed to [stats::medpolish()]
#'
#' @seealso [stats::medpolish()]
#' @export
medpol <- function(x, ...) UseMethod("medpol")

#' @export
medpol.default <- function(x, ..., trace.iter = FALSE, na.rm = TRUE)
  stats::medpolish(x, ..., trace.iter = trace.iter, na.rm = na.rm)

#' @describeIn medpol S3 method for xts object
#' @export
medpol.xts <- function(x, ...) {
  if (!is.null(dim(x)) && ncol(x) > 1) {
    as_list <- utils::getFromNamespace("as.list.xts", "xts")
    return(do.call(cbind, parallel::mclapply(as_list(x), medpol.xts, ...)))
  }
  i <- indexx(x, "md") == 229
  y <- x
  if (any(i)) {
    warning("Feb 29 data has been excluded for median polishing")
    y <- x[which(!i),]
  }
  years <- unique(indexx(y, "year"))
  if (length(years) < 2) stop("Number of years must be at least 2")
  y <- do.call(cbind, lapply(years, function(i) as.vector(y[as.character(i)])))
  m <- medpol(y, ...)
  p <- m$overall + outer(m$row, m$col, "+") # + m$residuals
  if (any(i)) {
    x[which(!i),] <- as.vector(p)
  } else {
    x <- xts::xts(as.vector(p), order.by = zoo::index(x))
  }
  return(x)
}


#' @description `na.medpolish` fills NA's by medpolish method.
#' @rdname medpol
#' @export
na.medpol <- function(x, ...) UseMethod("na.medpol")

#' @describeIn medpol S3 method for matrix
#' @export
na.medpol.matrix <- function(x, ...) {
  x[is.na(x)] <- medpol(x, ...)[is.na(x)]
  return(x)
}

#' @describeIn medpol S3 method for xts object
#' @export
na.medpol.xts <- function(x, ...) {
  p <- medpol.xts(x, ...)
  for (i in seq_len(ncol(p))) x[is.na(x[,i]), i] <- p[is.na(x[,i]), i]
  return(x)
}

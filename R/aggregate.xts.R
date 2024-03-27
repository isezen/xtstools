#' Aggregate xts object
#'
#' Aggregate `xts` object by month, day, year etc...
#'
#' @details Valid values for the argument `on` include:
#' "sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "yhour", "isdst".
#' `agg.*` functions are wrapper functions for overlapping time periods
#'
#' @param x,data An `xts` object
#' @param by A character string speficying overlapping time indexing to apply
#'           the `FUN`
#' @param formula a formula, such as y ~ x or cbind(y1, y2) ~ x1 + x2, where
#'                the y variables are numeric data to be split into groups
#'                according to the grouping x variables (usually factors).
#' @param FUN an argument of type `function`
#' @param subset an optional vector specifying a subset of observations to
#'               be used.
#' @param na.action a function which indicates what should happen when the
#'                  data contain NA values. The default is to ignore missing
#'                  values in the given variables.
#' @param ... additional arguments for `FUN`
#' @param wsize Window size to to take into account
#' @rdname aggregate.xts
aggregate.xts <- function(x, by, FUN, ..., wsize = 0) {
  wsize <- as.integer(wsize[1])
  list_by <- get_by_factor(zoo::index(x), by)[[2]]
  xx <- zoo::coredata(x)
  if (wsize > 0) {
    y <- as.data.frame(do.call(cbind, list_by))
    id <- y[,1]
    if (ncol(y) > 1)
      for (i in 2:ncol(y)) id <- id * 10^max(floor(log10(y[,i])) + 1) + y[,i]
    u <- unique(id)
    ret <- unique(y)
    ret[[deparse(substitute(x))]] <- lapply(1:length(u), function(i)
      apply(xx[which(id %in% wseq(u, i, wsize)), , drop = FALSE], 2, FUN, ...))
  } else {
    ret <- stats::aggregate(xx, by = list_by, FUN = FUN, ...)
  }
  rownames(ret) <- NULL
  a <- ret[order(ret[,1]),]
  if (!is.null(dim(a[,length(a)])) && ncol(a[,length(a)]) > 1) {
    la <- length(a)
    cn <- colnames(a[,la])
    if (is.null(cn)) cn <- seq_len(ncol(a[,la]))
    cn <- paste(colnames(a)[la], cn, sep = ".")
    a <- cbind(a[,-la, drop = FALSE], a[,la], deparse.level = 0)
    colnames(a)[la:length(a)] <- cn
  }
  return(a)
}

#' @rdname aggregate.xts
aggregate.formula <- function(formula, data, FUN, ..., subset,
                              na.action = stats::na.omit) {
  if (missing(formula) || !inherits(formula, "formula"))
    stop("'formula' missing or incorrect")
  if (length(formula) != 3L)
    stop("'formula' must have both left and right hand sides")
  is_xts <- inherits(data, "xts")
  if (is_xts) {
    melt <- "Series" %in% all.vars(formula)
    data <- fortify.zoo(data, melt = melt, by = formula, reduce = FALSE)
    data <- data[-1]
  }
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- m$FUN <- NULL
  m[[1L]] <- quote(stats::model.frame)
  if (formula[[2L]] == ".") {
    rhs <- as.list(attr(stats::terms(formula[-2L]), "variables")[-1])
    lhs <- as.call(c(quote(cbind), setdiff(lapply(names(data),
                                                  as.name), rhs)))
    formula[[2L]] <- lhs
    m[[2L]] <- formula
  }
  mf <- eval(m, parent.frame())
  lhs <- if (is.matrix(mf[[1L]])) {
    as.data.frame(mf[[1L]])
  }
  else mf[1L]
  a <- stats::aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
  if (is_xts & !is.null(dim(a[length(a)]))) {
    la <- length(a)
    cn <- colnames(a[,la])
    if (is.null(cn)) cn <- seq_len(ncol(a[,la]))
    cn <- paste(colnames(a)[la], cn, sep = ".")
    a <- cbind(a[,-la, drop = FALSE], a[,la], deparse.level = 0)
    colnames(a)[la:length(a)] <- cn
  }
  return(a)
}

#' @rdname aggregate.xts
#' @export
agg_mdh <- function(x, ...) aggregate.xts(x, by = "mdh", ...)
#' @rdname aggregate.xts
#' @export
agg_monday <- function(x, ...) aggregate.xts(x, by = c("Month", "mday"), ...)
#' @rdname aggregate.xts
#' @export
agg_yhour <- function(x, ...) aggregate.xts(x, by = "yhour", ...)
#' @rdname aggregate.xts
#' @export
agg_hour <- function(x, ...) aggregate.xts(x, by = "Hour", ...)
#' @rdname aggregate.xts
#' @export
agg_year <- function(x, ...) aggregate.xts(x, by = "Year", ...)
#' @rdname aggregate.xts
#' @export
agg_yday <- function(x, ...) aggregate.xts(x, by = "yday", ...)
#' @rdname aggregate.xts
#' @export
agg_mday <- function(x, ...) aggregate.xts(x, by = "mday", ...)
#' @rdname aggregate.xts
#' @export
agg_mon <- function(x, ...) aggregate.xts(x, by = "Month", ...)
#' @rdname aggregate.xts
#' @export
agg_wday <- function(x, ...) aggregate.xts(x, by = "wday", ...)
#' @rdname aggregate.xts
#' @export
agg_wdayhour <- function(x, ...) aggregate.xts(x, by = "wdayhour", ...)

#' @rdname aggregate.xts
#' @param period Length of period of xts to aggregate.
#' @export
agg_by_period <- function(x, period, FUN, ...) {
  by <- list(rep(1:period, length.out = nrow(x)))
  ret <- aggregate.xts(zoo::coredata(x), by = by, FUN = FUN, ...)
  rownames(ret) <- NULL
  ret
}

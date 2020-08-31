#' (de)trend xts object
#'
#' \code{trend} method creates model object for \code{loess} or \code{lm}
#' methods if \code{predict} is \code{FALSE}; otherwise predicted values are
#' calculated.
#' \code{detrend} method removes long term trend from \code{xts} or
#' \code{data.frame} object by \code{loess} or \code{lm} methods.
#' Multiple series are accepted and trends are calculated for each column of
#' objects.
#'
#' @param x an xts object
#' @param fun \code{"loess"} or \code{"lm"} string to choose the model.
#'            Can be abbreviated up to first two characters.
#' @param predict If \code{TRUE} predicted results are returned.
#' @param ... other parameters passed to \code{loess} or \code{lm} methods.
#' @param type De-trending type. \code{additive} or \code{multiplicative}.
#' @return For \code{trend} method, if \code{predict} is \code{FALSE}, model
#'         object is returned; otherwise predicted values return. For
#'         \code{detrend} method, trend is removed from \code{x} object and
#'         de-trended object with same class is returned.
#' @rdname trend
#' @export
trend <- function(x, ...) UseMethod("trend")

#' @describeIn trend S3 method for \code{zoo} object
#' @export
trend.zoo <- function(x, ..., fun = c("loess", "lm"), predict = TRUE) {
  if (!is.character(fun)) stop("fun must be character")
  dts <- zoo::index(x); i <- as.numeric(dts)
  t <- apply(x, 2, function(j)
    trend_internal(i, j, ..., fun = fun, predict = predict))
  return(if (predict) xts::xts(t, order.by = dts) else t)
}

#' @describeIn trend S3 method for \code{data.frame} object
#' @export
trend.data.frame <- function(x, ..., predict = TRUE) {
  swi <- startsWith(colnames(x), "index")
  if (!any(swi)) stop("x must have a column named index")
  if (!inherits(x$index, "POSIXt")) stop("index column must be POSIXt object")
  t <- trend(xts::xts(x[,!swi, drop = FALSE], order.by = x$index), ...,
             predict = predict)
  return(if (predict) cbind(x[,"index", drop = FALSE], as.data.frame(t)) else t)
}

trend_internal <- function(x, y, ..., fun = c("loess", "lm"), predict = TRUE,
                           type = c("additive", "multiplicative"),
                           control = stats::loess.control(trace.hat = "a")) {
  fun <- match.arg(fun)
  type <- match.arg(type)
  if (type == "multiplicative") {
    if (any(y[!is.na(y)] <= 0))
      stop("x values have to higher than zero for multiplicative trend calculation")
    y <- log(y)
  }
  data <- data.frame(x = x, y = y)
  model <- if (fun == "lm") stats::lm(y ~ x, data, ...)
  else stats::loess(y ~ x, data, ..., control = control)
  if (predict) {
    df <- stats::predict(model, newdata = data)
    if (type == "multiplicative") df <- exp(df)
  } else df <- model
  df
}

#' @rdname trend
#' @export
detrend <- function(x, fun = c("loess", "lm"), ...,
                    type = c("additive", "multiplicative")) {
  if (!inherits(x, "zoo")) stop("x must be an zoo or xts object")
  t <- trend(x, fun = fun, ..., predict = TRUE, type = type)
  if (type == "additive") x - t else x/t
}

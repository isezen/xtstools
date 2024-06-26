#' (de)trend xts object
#'
#' `trend` method creates model object for `loess` or `lm`
#' methods if `predict` is `FALSE`; otherwise predicted values are
#' calculated.
#' `detrend` method removes long term trend from `xts` or
#' `data.frame` object by `loess` or `lm` methods.
#' Multiple series are accepted and trends are calculated for each column of
#' objects.
#'
#' @param x an xts object
#' @param fun `"loess"` or `"lm"` string to choose the model.
#'            Can be abbreviated up to first two characters.
#' @param predict If `TRUE` predicted results are returned.
#' @param ... other parameters passed to `loess` or `lm` methods.
#' @param type De-trending type. `additive` or `multiplicative`.
#' @return For `trend` method, if `predict` is `FALSE`, model
#'         object is returned; otherwise predicted values return. For
#'         `detrend` method, trend is removed from `x` object and
#'         de-trended object with same class is returned.
#' @rdname trend
#' @export
trend <- function(x, ...) UseMethod("trend")

#' @describeIn trend S3 method for `zoo` object
#' @export
trend.zoo <- function(x, ..., fun = c("loess", "lm", "med"), predict = TRUE) {
  if (!is.character(fun)) stop("fun must be character")
  dts <- zoo::index(x); i <- as.numeric(dts)
  t <- apply(x, 2, function(j)
    trend_internal(i, j, ..., fun = fun, predict = predict))
  return(if (predict) xts::xts(t, order.by = dts) else t)
}

#' @describeIn trend S3 method for `data.frame` object
#' @export
trend.data.frame <- function(x, ..., predict = TRUE) {
  swi <- startsWith(colnames(x), "index")
  if (!any(swi)) stop("x must have a column named index")
  if (!inherits(x$index, "POSIXt")) stop("index column must be POSIXt object")
  t <- trend(xts::xts(x[,!swi, drop = FALSE], order.by = x$index), ...,
             predict = predict)
  return(if (predict) cbind(x[,"index", drop = FALSE], as.data.frame(t)) else t)
}

#' @importFrom utils capture.output modifyList
trend_internal <- function(x, y, ..., fun = c("loess", "lm", "med"), predict = TRUE,
                           type = c("additive", "multiplicative")) {
  fun <- match.arg(fun)
  type <- match.arg(type)
  if (type == "multiplicative") {
    if (any(y[!is.na(y)] <= 0))
      stop("x values have to higher than zero for multiplicative trend calculation")
    y <- log(y)
  }
  data <- data.frame(x = x, y = y)
  if (fun == "lm") {
    model <- stats::lm(y ~ x, data, ...)
  } else if (fun == "loess") {
    dots <- list(...)
    if ("width" %in% names(dots)) {
      dots$span <- dots$width/length(zoo::na.trim(y))
      dots$width <- NULL
    }
    dots <- utils::modifyList(list(formula = y ~x, family = "symmetric",
                              control = stats::loess.control(trace.hat = "a")), dots)
    model <- do.call(stats::loess, dots)
  } else if (fun == "med") {
    y <- as.vector(y)
    dots <- utils::modifyList(list(y = y, width = 0.75 * length(y)), list(...))
    utils::capture.output(model <- do.call(robfilter::med.filter, dots))
  } else {
    stop("Undefined function")
  }
  if (predict) {
    df <- if (fun == "med") {
      model$level[,1]
    } else {
      stats::predict(model, newdata = data)
    }
    if (type == "multiplicative") df <- exp(df)
  } else df <- model
  df
}

#' @rdname trend
#' @export
detrend <- function(x, fun = c("loess", "lm", "med"), ...,
                    type = c("additive", "multiplicative")) {
  type <- match.arg(type)
  if (!inherits(x, "zoo")) stop("x must be an zoo or xts object")
  t <- trend(x, fun = fun, ..., predict = TRUE, type = type)
  if (type == "additive") x - t else x/t
}

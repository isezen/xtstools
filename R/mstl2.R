#' Multiple seasonal decomposition
#'
#' Decompose a time series into seasonal, trend and remainder components.
#' \code{robust} is \code{TRUE} as different than \code{\link[forecast]{mstl}}.
#' Also Boxcox transformation is accomplished by
#' \code{\link{boxcox}} function.
#'
#' @param x Univariate time series of class \code{msts} or \code{ts}.
#' @param lambda BoxCox transformation coefficient. If \code{"auto"}, lambda is
#' automatically selected. If \code{NULL} no transformation is done.
#' @param s.window Seasonal windows to be used in the  decompositions. If scalar,
#' the same value is used for all seasonal components. Otherwise, it should be a vector
#' of the same length as the number of seasonal components.
#' @param ... Other arguments are passed to \code{\link[stats]{stl}}.
#' @param iterate Number of iterations to use to refine the seasonal component.
#' @param type Type of decomposition. \code{additive} or \code{multiplicative}.
#'
#' @seealso \code{\link[stats]{stl}}, \code{\link[forecast]{mstl}}
#' @export
mstl2 <- function(x, lambda = NULL, s.window = 13, ..., iterate = 2,
                  type = c("additive", "multiplicative")) {
  if (!requireNamespace("forecast", quietly = TRUE))
    stop("forecast package required for this function")
  type <- match.arg(type)
  if (!is.null(lambda)) {
    if (lambda == "auto") lambda <- boxcox.lambda(x)
    x <- boxcox(x, lambda)
  }
  if (type == "multiplicative") {
    if (any(x <= 0))
      stop("x values havet o higher than zero for multiplicative decomposition")
    x <- log(x)
  }
  dots <- utils::modifyList(list(x = x, lambda = NULL, iterate = iterate,
                                 s.window = s.window, robust = TRUE), list(...))
  st <- do.call(forecast::mstl, dots)
  cn <- colnames(st)
  sp <- attr(st, "seasonal.periods")
  cls <- class(st)
  t <- forecast::trendcycle(st)
  s <- forecast::seasonal(st)
  if (length(as.vector(s)) == 0) s <- 0
  f <- t + if (!is.null(dim(s))) rowSums(s) else s
  st <- cbind(st, f)
  if (type == "multiplicative") st <- exp(st) # reverse log transform
  colnames(st) <- c(cn, "Fitted")
  attr(st, "seasonal.periods") <- sp
  class(st) <- cls
  if (!is.null(lambda)) attr(st, "lambda") <- lambda
  ssacf <- function(x) sum(stats::acf(x, na.action = stats::na.omit,
                                      plot = FALSE)$acf^2)
  attr(st, "err") <- sapply(list(mae = mae, rmse = rmse, ssacf = ssacf),
                            function(f) f(st[,"Remainder"]))
  substr(type, 1, 1) <- toupper(substr(type, 1, 1))
  attr(st, "type") <- type
  return(st)
}

#' Plot function for mstl object
#' @param labels character of length 4 giving the names of the component
#'               time-series.
#' @param set.pars settings for par(.) when setting up the plot.
#' @param main plot main title.
#' @param range.bars logical indicating if each plot should have a bar at its
#'                   right side which are of equal heights in user coordinates.
#' @param col.range Colour to be used for the range bars, if plotted. Note this
#'                  appears after ... and so cannot be abbreviated.
#' @rdname mstl2
#' @export
plot.mstl <- function(x, labels = colnames(X),
                      set.pars = list(mar = c(0, 6, 0, 6),
                                      oma = c(6, 0, 4, 0), tck = -0.01,
                                      mfrow = c(nplot, 1)),
                      main = NULL, range.bars = TRUE, ...,
                      col.range = "light gray") {
  sers <- x
  ncomp <- ncol(sers)
  X <- sers
  nplot <- ncomp
  if (range.bars)
    mx <- min(apply(rx <- apply(X, 2, range), 2, diff))
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush())
  if (length(set.pars)) {
    oldpar <- do.call(graphics::par, as.list(names(set.pars)))
    on.exit(graphics::par(oldpar), add = TRUE)
    do.call(graphics::par, set.pars)
  }
  type <- attr(x, "type")
  for (i in 1L:nplot) {
    plot(X[, i], type = if (i < nplot + 1) "l" else "h", xlab = "", ylab = "",
         axes = FALSE)
    if (i == 1 && is.character(type)) graphics::mtext(type, line = 0, adj = 1)
    if (range.bars) {
      dx <- 1/64 * diff(ux <- graphics::par("usr")[1L:2])
      y <- mean(rx[, i])
      graphics::rect(ux[2L] - dx, y + mx/2, ux[2L] - 0.4 * dx, y -
                       mx/2, col = col.range, xpd = TRUE)
    }
    if (i == 1 && !is.null(main))
      graphics::title(main, line = 2, outer = graphics::par("oma")[3L] > 0)
    if (i == nplot) graphics::abline(h = 0)
    graphics::box()
    right <- i %% 2 == 0
    graphics::axis(2, labels = !right)
    graphics::axis(4, labels = right)
    graphics::axis(1, labels = i == nplot)
    graphics::mtext(labels[i], side = 2, 3)
  }
  graphics::mtext("time", side = 1, line = 3)
  invisible()
}

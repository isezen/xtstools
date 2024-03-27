#' Summary statistics on a vector
#'
#' @param x A numveric vector.
#' @param B number of bootstrap resamples for `mean_cl_boot`.
#' @param q 2 quantiles to calculate.
#' @param na.rm Do you want to remove `NA`'s? Default is `TRUE`.
#' @param ... Other parameters passed to the functions.
#' @rdname summary_stats
#' @export
mean_cl_normal <- function(x, ...) {
  structure(Hmisc::smean.cl.normal(x, ...), dim = c(1, 3), names = NULL,
            dimnames = list(NULL, c("y", "ymin", "ymax")))
}
#' @rdname summary_stats
#' @export
mean_cl_boot <- function(x, B = 10, ...) {
  structure(Hmisc::smean.cl.boot(x, B = B, ...), dim = c(1, 3), names = NULL,
            dimnames = list(NULL, c("y", "ymin", "ymax")))
}
#' @rdname summary_stats
#' @export
median_quantile <- function(x, q = c(0.25, 0.75), na.rm = TRUE) {
  structure(c(stats::median(x, na.rm = na.rm),
              stats::quantile(x, probs = q[1], na.rm = na.rm),
              stats::quantile(x, probs = q[2], na.rm = na.rm)),
            dim = c(1, 3), names = NULL,
            dimnames = list(NULL, c("y", "ymin", "ymax")))
}

#' Seasonal Analysis
#'
#' Calculate daily, weekly, annual means over hours and long term means over days.
#'
#' @param x An hourly `xts` object
#' @param FUNS Functions to be applied over `x`
#' @param ... Unused for now.
#'
#' @import ggplot2
#' @export
seasonal_stats <- function(x, ...) UseMethod("seasonal_stats")

#' @describeIn seasonal_stats S3 Method for `xts`
#' @export
seasonal_stats.xts <- function(x, FUNS = c("mean_cl_boot", "median_quantile"),
                               ...) {
  if (ncol(x) > 1) stop("x must be univariate time series")
  if (is.null(colnames(x))) colnames(x) <- deparse(substitute(x))
  if (sum(is.na(x)) > 0) {
    warnings("NA values are omitted")
    x <- zoo::na.trim(x)
  }

  cn <- c("x", "xmin", "xmax")
  # long-term daily means
  lt <- do.call(rbind, lapply(FUNS, function(f) {
    r <- xts::apply.daily(x, match.fun(f)); i <- zoo::index(r)
    r <- data.frame(Time = i, zoo::coredata(r), Statistics = factor(f))
    colnames(r)[2:4] <- cn
    return(r)
  }))
  # ------------------------------------------------------------------------------
  # Daily
  dh <- do.call(rbind, lapply(FUNS, function(f) {
    r <- agg_hour(x, match.fun(f))
    cbind(Hours = r[,1], Statistics = factor(f), r[,-1])
  }))
  colnames(dh)[3:5] <- cn
  # Week
  wd <- agg_wday(x, mean)
  colnames(wd)[2] <- cn[1]
  wd$WeekDay <- (wd$WeekDay - 1) * 24 + 12
  get_trend <- function(x) {
    as.vector(forecast::trendcycle(stats::stl(stats::ts(x, frequency = 24), 7)))
  }
  wdh <- do.call(rbind, lapply(FUNS, function(f) {
    i <- 0:167
    r <- sapply(agg_wdayhour(x, match.fun(f))[,-1], get_trend)
    cbind(WeekDay = i, Statistics = factor(f), as.data.frame(r))
  }))
  colnames(wdh)[3:5] <- cn
  # Annual
  get_trend <- function(x) {
    s <- stats::stl(stats::ts(x, frequency = 24), 7)
    s <- stats::stl(stats::ts(as.vector(forecast::trendcycle(s) +
                                          forecast::remainder(s)),
                              frequency = 168), 7)
    as.vector(forecast::trendcycle(s))
  }
  yh <- do.call(rbind, lapply(FUNS, function(f) {
    a <- agg_mdh(x, length)
    a <- a[which(a[,2] == 1), 1]
    if (length(a) > 0) {
      x2 <- x[which(!(indexx(x, "mdh") %in% a))]
      warning(a, " have single value. It will be removed")
    } else x2 <- x
    # mdh <- agg_mdh(x2, match.fun(f))
    # r <- sapply(mdh[,-1], function(x) {
    #   x <- get_trend(x)
    #   data.frame(0:(length(x) - 1), x)
    # })
    # r <- cbind(r[[1]][,1], sapply(r, function(r) r[,2]))
    # return(cbind(as.data.frame(r), Statistics = factor(f)))
    mdh <- agg_mdh(x2, match.fun(f))
    r <- sapply(mdh[,-1], get_trend)
    cbind(mdh[1], Statistics = factor(f), r)
  }))
  colnames(yh)[1:4] <- c("Months", cn)
  m <- agg_mon(x, mean)
  colnames(m)[2] <- cn[1]
  m$mon <- monthbreak() + 15 * 24

  class(dh) <- append("daily", class(dh))
  week <- list(day = wd, hour = wdh)
  class(week) <- append("weekly", class(week))
  annual <- list(month = m, hour = yh)
  class(annual) <- append("annual", class(annual))
  class(lt) <- append("longterm", class(lt))
  r <- list(daily = dh, week = week, annual = annual, longterm = lt)
  class(r) <- append("seasonal_analysis", class(r))
  return(r)
}

#' aes_str <- function(x, n = 4) {
#'   c <- colnames(x)
#'   if (length(c) < n) c <- c(c, rep(c[length(c)], n - length(c)))
#'   args <- c("x", "y", "col", "fill")
#'   c <- as.list(c)
#'   names(c) <- args[1:length(c)]
#'   do.call(ggplot2::aes_string, c)
#' }
#'
#' scale_data <- function(x, n = 50) {
#'   cn <- colnames(x)
#'   x2 <- apply(x[,2:4], 2, function(y) {
#'     df <- data.frame(x[,1], x = y, Statistics = x[,5])
#'     spline_scale(df, 0:(nrow(df)), df[,3], length.out = n)
#'   })
#'   x <- sapply(x2, function(x) x[,2])
#'   x <- cbind(x2$x[,1], as.data.frame(x), x2$x$Statistics)
#'   colnames(x) <- cn
#'   return(x)
#' }
#' #' @export
#' plot.daily <- function(daily) {
#'   return(ggplot(daily, aes_str(daily[c(1,2,5)])) +
#'     geom_line() +
#'     geom_ribbon(aes_string(ymin = "xmin", ymax = "xmax"), col = NA,
#'                 alpha = 0.3) +
#'     scale_x_continuous(breaks = seq(0, 23, 2), minor_breaks = NULL))
#' }
#' #' @export
#' plot.weekly <- function(week) {
#'   wdh <- scale_data(week$hour)
#'   cn <- colnames(week$day)
#'   return(ggplot(wdh, aes_str(wdh[c(1,2,5)])) +
#'     geom_ribbon(aes_string(ymin = "xmin", ymax = "xmax"), col = NA,
#'                 alpha = 0.3) +
#'     geom_line() +
#'     geom_point(aes_string(cn[1], cn[2]), data = week$day, shape = 3,
#'                inherit.aes = FALSE) +
#'     scale_x_continuous(breaks = seq(0, 167, 48),
#'                        labels = weekday.abb[seq(1, 7, 2)]))
#' }
#' #' @export
#' plot.annual <- function(annual) {
#'   yh <- scale_data(annual$hour, 300)
#'   cn <- colnames(annual$month)
#'   return(ggplot(yh, aes_str(yh[c(1,2,5)])) +
#'     geom_ribbon(aes_string(ymin = "xmin", ymax = "xmax"), col = NA,
#'                 alpha = 0.3) +
#'     geom_line() +
#'     geom_point(aes_string(cn[1], cn[2]), data = annual$month, shape = 3,
#'                inherit.aes = FALSE) +
#'     scale_x_continuous(breaks = month.break()[seq(1, 12, 2)],
#'                        labels = month.abb[seq(1, 12, 2)]))
#' }
#' #' @export
#' plot.longterm <- function(lt) {
#'   Statistics <- lt$Statistics
#'   Time <- lt$Time
#'   lt <- c(lt[which(lt$Statistics == "meancl"), "xmax"],
#'           lt[which(lt$Statistics == "medianq"), "x"])
#'   lt <- data.frame(Time = Time, x = lt, Statistics)
#'   max <- quantile(lt$x, 0.999, na.rm = TRUE)
#'   min <- c(quantile(lt$x, 0, na.rm = TRUE))
#'   lt[which(lt$x > max),2] <- max
#'   return(ggplot2::ggplot(lt, aes_str(lt)) +
#'            ggplot2::geom_line(size = 0.2) + ggplot2::ylim(min, max))
#' }
#' #' @export
#' plot.seasonal_analysis <- function(a, base_size = 10, base_family = "sans",
#'                                    ylab = NULL,
#'                                    hjust = c(-0.5, -0.1, -0.5, -0.1),
#'                                    vjust = c(0.5, 0.5, -0.5, -0.5),
#'                                    ncol = 2, nrow = 2) {
#'   legend_text <- c("Mean + CI (95%)", "Median + IQR")
#'   p <- lapply(a, plot)
#'   if (!is.null(ylab)) p <- ggappend(p, ylab(ylab))
#'   p <- ggappend(p,
#'     scale_color_brewer(palette = "Set1", name = "",
#'                                 labels = legend_text),
#'     scale_fill_brewer(palette = "Set1", name = "",
#'                                labels = legend_text),
#'     ggthemes::theme_pander(base_size = base_size, base_family = base_family),
#'     theme(axis.title.y = element_text(angle = 0, vjust = 0.5)))
#'
#'   ret <- tryCatch({
#'     pdf(NULL)
#'     ggpubr::ggarrange(plotlist = p, ncol =  ncol, nrow = nrow,
#'                       labels = paste0("(", letters[1:4], ")"), legend = "top",
#'                       hjust = hjust, vjust = vjust, common.legend = TRUE,
#'                       font.label = list(size = base_size, face = "plain",
#'                                         family = base_family))
#'   },
#'   finally = {dev.off()})
#'   return(ret)
#' }

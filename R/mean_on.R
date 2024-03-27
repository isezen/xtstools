#' Calculate Mean on periods
#'
#' @param x an `xts` or `data.frame` object.
#' @param on The periods to find as character string
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from
#'             each end of x before the mean is computed. (see [mean])
#' @param na.rm a logical value indicating whether NA values should be
#'              stripped before the computation proceeds or a value between
#'              (0, 100) to decide percent of NA values.
#' @param ... Not used.
#'
#' @examples
#' time_stamps <- seq(from = as.POSIXct("2024-01-01 00:00:00"),
#'                    to = as.POSIXct("2024-01-31 23:00:00"), by = "hour")
#' hourly_data <- xts::xts(seq_along(time_stamps), order.by = time_stamps,
#'                         dimnames = list(NULL, "x"))
#' mean_on(hourly_data, on = "days")
#' @export
mean_on <- function(x, on, trim, na.rm, ...) UseMethod("mean_on")

#' @describeIn mean_on S3 method for xts
#' @export
mean_on.xts <- function(x, on, trim = 0, na.rm = FALSE, ...) {
  ep <- xts::endpoints(x, on = on)
  per <- mode2(ep[2:length(ep)] - ep[1:(length(ep) - 1)])
  if (is.numeric(na.rm)) {
    non_na_count <- per * (100 - na.rm)/100
    x2 <- xts::period.apply(x, INDEX = ep, FUN = function(x) {
      apply(x, 2, function(x) {
        x <- x[!is.na(x)]
        ifelse(length(x) < non_na_count, NA, base::mean(x, trim, TRUE))
      })
    })
  } else {
    x2 <- xts::period.apply(x, INDEX = ep, base::mean, trim = trim,
                            na.rm = na.rm)
  }
  zoo::index(x2) <- if (on == "months")
    zoo::as.yearmon(zoo::index(x2), format = "%Y %m") else
      as.POSIXct(trunc(zoo::index(x2), on))
  return(zoo::na.trim(x2, is.na = "all"))
}

#' @describeIn mean_on S3 method for data.frame
#' @export
mean_on.data.frame <- function(x, on, trim = 0, na.rm = FALSE, ...) {
  i <- which(sapply(sapply(x, class), function(x) x[1] == "POSIXct"))
  if (length(i) < 1) stop("x data.frame must have a at least one POSIXct column")
  i <- i[1]
  as.data.frame(mean_on(xts::xts(x[,-i], x[,i]), on, trim, na.rm))
}

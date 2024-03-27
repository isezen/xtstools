#' Calculate mean at a time step
#'
#' Calculate means on specific time steps in a `delta` time window. `at`
#' must be in the interval of max and min dates of `x`.
#'
#' @param x An xts object
#' @param at Dates that means will be calculated. Can be [POSIXt] or
#'           [character] vector.
#' @param delta Time window to the left and right of `at` parameter.
#'              for instance, if `delta = 2`, measurements at
#'              22, 23, 00, 01 and 02 is used to calculate the mean at 00.
#' @param fill If `TRUE`, missing values are filled with `NA`.
#'
#' @return Mean values at `at`
#'
#' @examples
#' time_stamps <- seq(from = as.POSIXct("2024-01-01 00:00:00"),
#'                    to = as.POSIXct("2024-01-02 23:00:00"), by = "hour")
#' at <- seq(from = min(time_stamps), to = max(time_stamps), by = "12 hours")
#' hourly_data <- xts::xts(seq_along(time_stamps), order.by = time_stamps,
#'                         dimnames = list(NULL, "x"))
#' mean_at(hourly_data, at = at)
#' # missing observations
#' mean_at(hourly_data[-(13:14),], at = as.POSIXct("2024-01-01 12:00:00"), fill = TRUE)
#' @export
mean_at <- function(x, at, delta = 2, fill = FALSE) UseMethod("mean_at")

#' @describeIn mean_at Calculate means on specific dates in a `delta` time.
#' @importFrom lubridate tz
#' @export
mean_at.xts <- function(x, at, delta = as.difftime(2, units = "hours"),
                       fill = FALSE) {
  if (is.character(at)) {
    at <- as.POSIXct(at, tz = lubridate::tz(x))
  }
  if (!lubridate::is.POSIXt(at))
    stop("'at' can be a proper character or POSIXt vector")
  if (!inherits(delta, "difftime")) {
    delta <- as.difftime(delta, units = "hours")
  }
  d <- zoo::index(x)
  ir <- range(d)
  ir <- c(ir[1] - delta, ir[2] + delta)
  lubridate::tz(ir) <- "GMT"
  if (fill) x <- fill.missing(x, inrange = ir)
  if (delta > 0) {
    if (ncol(x) == 1) {
      rx <- mapply(function(i, j) {
        mean(x[paste0(i, "/", j)], na.rm = TRUE)
      } ,at - delta, at + delta)
    } else {
      rx <- t(mapply(function(i, j) {
        colMeans(x[paste0(i, "/", j)], na.rm = TRUE)
      } ,at - delta, at + delta))
    }
    rx_at <- xts::xts(rx, at[at %in% zoo::index(x)])
    rx_at <- xts::merge.xts(rx_at, xts::xts(NULL, at), all = TRUE)
    colnames(rx_at) <- colnames(x)
  } else {
    rx_at <- x[at]
  }
  rx_at[is.nan(rx_at)] <- NA
  return(round(rx_at, 1))
}

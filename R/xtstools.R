#' xtstools: Useful time-series tools
#'
#' scalex provides functions for climatological standardization
#'
#' @docType package
#' @author İsmail SEZEN <sezenismail@gmail.com>
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @useDynLib xtstools, .registration = TRUE
#' @name xtstools-package
"_PACKAGE"

#' Time-series variables
#'
#' Some general purpose variables which might be useful for time-series
#' manipulations.
#'
#' @rdname tsvars
#' @format `weekday.name`: Name of weekdays
#' @export
weekday.name <- weekdays(seq(as.Date("2008-01-07"), by = 1, len = 7), FALSE)

#' @rdname tsvars
#' @format `weekday.name`: Short name of weekdays
#' @export
weekday.abb <- weekdays(seq(as.Date("2008-01-07"), by = 1, len = 7), TRUE)

#' @rdname tsvars
#' @format `ndays`: Number of days in months
#' @export
ndays <- c(31, 29, 31, 30, 30, 30, 31, 31, 30, 31, 30, 31)
names(ndays) <- month.abb

#' @rdname tsvars
#' @description `monthbreak` function calculates start of each month.
#' @param mult Multiplier for month break calculation
#' @export
monthbreak <- function(mult = 24) {
  structure(c(0, (cumsum(ndays) * mult)[-length(ndays)]),
            names = month.abb)
}

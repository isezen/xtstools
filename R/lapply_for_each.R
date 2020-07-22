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

#' Error Metrics
#'
#' @param actual Actual data
#' @param predicted Predicted data
#' @rdname metrics
#' @export
ae <- function(actual, predicted = 0) abs(actual - predicted)

#' @rdname metrics
#' @export
mae <- function(actual, predicted = 0) mean(ae(actual, predicted), na.rm = TRUE)

#' @rdname metrics
#' @export
se <- function(actual, predicted = 0) (actual - predicted)^2

#' @rdname metrics
#' @export
mse <- function(actual, predicted = 0) mean(se(actual, predicted), na.rm = TRUE)

#' @rdname metrics
#' @export
rmse <- function(actual, predicted = 0) sqrt(mse(actual, predicted))

#' Slice Array
#'
#' Slice array by `margin` and margin `value` .
#'
#' @param x An `array`
#' @param margin Margin for slicing.
#' @param value Margin value for slicing.
#' @param drop Drop results?
#'
#' @return Numeric sliced object.
#' @keywords internal
slice_array <- function(x, margin, value, drop = TRUE) {
  indices <- rep(list(bquote()), length(dim(x)))
  if (missing(value)) value <- seq_len(dim(x)[margin])
  indices[[margin]] <- value
  call <- as.call(c(
    list(as.name("["), quote(x)),
    indices,
    list(drop = drop)))
  eval(call)
}

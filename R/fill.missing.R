#' Fill Missing Data
#'
#' Fill missing time steps in an [xts] series and complete `NA` values
#' if requested.
#'
#' @param x An xts object.
#' @param fill see [zoo::na.fill].
#' @param inrange Range of indices to be filled with `NA`.
#' @param ... Additional arguments to be passed to [zoo::na.fill].
#'
#' @return filled object
#'
#' @export
fill.missing <- function(x, fill = NA, inrange = range(x), ...)
  UseMethod("fill.missing")

#' @rdname fill.missing
#' @export
fill.missing.xts <- function(x, fill = NA,
                             inrange = range(zoo::index(x)), ...) {
  d <- zoo::index(x)
  moded <- mode_int(diff(d))
  dft <- diff(d)[which(diff(d) == moded)[1]]
  mind <- min(inrange, na.rm = TRUE)
  maxd <- max(inrange, na.rm = TRUE)
  d <- seq(mind, maxd, by = dft)
  x <- xts::merge.xts(x, xts::xts(NULL, d), all = TRUE)
  if (length(fill) == 1 && is.na(fill)) return(x)
  if (length(fill) == 1) fill <- rep(fill, 3)
  x <- zoo::na.fill(x, fill = fill, ...)
  return(x)
}

#' @rdname fill.missing
#' @importFrom xts as.xts
#' @export
fill.missing.zoo <- function(x, ...) {
  fill.missing(xts::as.xts(x), ...)
}

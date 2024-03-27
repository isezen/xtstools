#' Alone Value Index
#'
#' This function returns values/indices of values between at least two
#' `NA`'s.
#'
#' @param x An R object (`vector`, `matrix` or `data.frame`)
#' @param flat If `x` is a matrix-like object, it's interpreted as vector
#'             and indices are returned according to this.
#' @param ... Unused.
#' @export
alone <- function(x, ...) UseMethod("alone")

#' @export
alone.default <- function(x, flat = FALSE, ...) {
  if (is.data.frame(x) && flat) {
    flat <- FALSE
    warning("If x is data.frame, flat must be FALSE")
  }
  i <- alone_index(x, flat = flat)
  v <- if (is.list(i))
    return(lapply(seq_len(length(i)), function(j) x[i[[j]], j]))
  v <- unclass(x)[i]
  if (!is.null(names(i))) names(v) <- names(i)
  return(v)
}

#' @describeIn alone S3 method for `formula` interface
#' @param formula a formula, such as y ~ x or cbind(y1, y2) ~ x1 + x2, where
#'                the y variables are numeric data to be split into groups
#'                according to the grouping x variables (usually factors).
#' @param data A `data.frame`
#' @export
alone.formula <- function(formula, data, ...) {
  data[alone_index(formula, data), ]
}

#' @rdname alone
#' @export
alone_index <- function(x, ..., flat = FALSE) UseMethod("alone_index")

#' @export
alone_index.default <- function(x, flat = FALSE, ...) {
  if (is.null(dim(x))) {
    r <- rle(is.na(x))
    return(cumsum(r$lengths)[which(!r$values & r$lengths == 1)])
  }
  i <- if (is.matrix(x)) apply(x, 2, alone_index) else sapply(x, alone_index)
  if (is.matrix(i)) i <- as.data.frame(i)
  if (flat) {
    n <- cumsum(c(0, rep(nrow(x), ncol(x) - 1)))
    i <- mapply(`+`, i, n)
    if (is.list(i)) i <- do.call(c, i)
    return(as.vector(i))
  }
  return(i)
}

#' @export
alone_index.zoo <- function(x, flat = FALSE, ...) {
  alone_index(zoo::coredata(x), flat)
}

#' @describeIn alone S3 method for `formula` interface
#' @param formula a formula, such as y ~ x or cbind(y1, y2) ~ x1 + x2, where
#'                the y variables are numeric data to be split into groups
#'                according to the grouping x variables (usually factors).
#' @param data A `data.frame`
#' @export
alone_index.formula <- function(formula, data, ...) {
  a <- eval_formula(formula, data, na.action = stats::na.pass)
  by <- a$by
  n <- stats::na.omit(as.vector(tapply(seq_len(nrow(a$x)), by, length)))
  n <- c(0, cumsum(n)[seq_len(length(n) - 1)])
  if (ncol(a$x) == 1) a$x <- a$x[[1]]
  i <- lapply(split(a$x, a$by), alone_index)
  if (length(by) > 1) i[sapply(i, is.null)] <- NULL
  i <- mapply(`+`, i, n)
  if (is.list(i)) i <- do.call(c, i)
  return(as.vector(i))
}

#' Reduce xts/zoo object
#'
#' This function reduces \code{xts}/\code{zoo}/\code{data.frame} object by the
#' second smallest period in the series if \code{length} of series is greater
#' than a specified threshold. \code{data.frame} must have at least one
#' \code{POSIXt} and one \code{numeric} column. If \code{data.frame} has a
#' \code{factor} column, evaluation is done by considering \code{factor} levels.
#'
#' @param x An \code{xts}/\code{zoo}/\code{data.frame} object.
#' @param formula Formula to be evaluated.
#' @param data \code{data.frame} for the formula.
#' @param merge if \code{merge = TRUE} and \code{x} has multiple \code{numeric}
#'              columns, the result indices are merged.
#' @param which.i If \code{TRUE}, returns indices.
#' @param ... Unused
#' @return A list of indices/values of \code{xts}/\code{zoo}/\code{data.frame}
#'         objects. If \code{merge = TRUE}, values in the list are merged.
#'
#' @export
reduce <- function(x, ..., which.i = FALSE) UseMethod("reduce")

#' @describeIn reduce S3 method for \code{zoo} object
#' @export
reduce.zoo <- function(x, merge = TRUE, which.i = FALSE, ...) {
  index_xts <- utils::getFromNamespace("index.xts", "xts")
  i <- reduce_index(zoo::coredata(x), as.POSIXct(index_xts(x)), merge)
  if (which.i) return(i)
  if (merge) return(x[i])
  z <- lapply(seq_len(length(i)), function(j) x[i[[j]], j])
  names(z) <- names(x)
  return(z)
}

#' @describeIn reduce S3 method for \code{data.frame}
#' @export
reduce.data.frame <- function(x, merge = TRUE, which.i = FALSE, ...) {
  ti <- sapply(x, function(x) inherits(x, "POSIXt") | inherits(x, "Date"))
  if (sum(ti) != 1) stop("x must have only a 'POSIXt' based index column")
  di <- sapply(x, inherits, "numeric")
  if (sum(di) < 1) stop("x must have at least one 'numeric' column")
  fi <- sapply(x, inherits, "factor")
  if (any(fi)) {
    i <- reduce_long_df(x[, ti | di], x[, fi, drop = FALSE])
    if (which.i) return(i)
    return(x[i, ])
  }
  #
  i <- reduce_index(x[, di, drop = FALSE], x[, ti], merge)
  if (which.i) return(i)
  if (merge) return(x[i, ])
  data <- x[, ti | di, drop = FALSE]
  ti <- which(ti); di <- which(di)
  z <- lapply(seq_len(length(i)), function(j) data[i[[j]], c(ti, di[j])])
  names(z) <- names(x)[di]
  return(z)
}

#' @describeIn reduce S3 method for \code{formula}
#' @export
reduce.formula <- function(formula, data, which.i = FALSE, ...) {
  ti <- sapply(data, inherits, "POSIXt")
  if (sum(ti) != 1) stop("x must have only a 'POSIXt' based index column")
  a <- eval_formula(formula, data, na.action = stats::na.pass)
  a$x[, 1] <- as.POSIXct(a$x[, 1], attr(data[, 1], "tzone"),
                         origin = "1970-01-01")
  i <- reduce_long_df(a$x, a$by)
  if (which.i) return(i)
  return(data[i, ])
}

# x must be matrix-like object
# index is POSIXt vector
# merge is TRUE or FALSE
reduce_index <- function(x, index, merge) {
  ep_max <- length(xts::endpoints(index, on = "seconds")) - 1
  ep_limit <- getOption("reduce_limit", ep_max)
  for (p in c("minutes", "hours", "days",
              "weeks", "months", "quarters", "years")) {
    ep <- xts::endpoints(index, on = p)
    if (length(ep) - 1 < ep_limit) break;
  }
  i <- apply(x, 2, function(x) {
    sapply(1:(length(ep) - 1), function(y) {
      z <- x[(ep[y] + 1):ep[y + 1]]
      if (all(is.na(z))) return(c(1, 1))
      c(which.max(z), which.min(z))
    })
  }) + rep(ep[-length(ep)], each = 2)
  if (merge) return(unique(as.vector(i)))
  i <- apply(i, 2, unique)
  if (is.matrix(i)) i <- as.data.frame(i)
  names(i) <- colnames(x)
  return(i)
}

reduce_long_df <- function(data, by) {
  j <- split(seq_len(nrow(data)), by, lex.order = TRUE)
  i <- lapply(j, function(i) {
    d <- data[i,]
    if (any(dim(d) == 0)) return(NULL)
    reduce_index(d[, 2, drop = FALSE], d[, 1], TRUE)
  })
  if (length(by) > 1) i[sapply(i, is.null)] <- NULL
  i <- mapply(function(x, y) y[x], i, j)
  if (is.list(i)) i <- do.call(c, i)
  return(as.vector(i))
}

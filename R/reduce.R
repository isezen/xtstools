#' Reduce xts/zoo object
#'
#' This function reduces `xts`/`zoo`/`data.frame` object by the
#' second smallest period in the series if `length` of series is greater
#' than a specified threshold. `data.frame` must have at least one
#' `POSIXt` and one `numeric` column. If `data.frame` has a
#' `factor` column, evaluation is done by considering `factor` levels.
#'
#' @param x An `xts`/`zoo`/`data.frame` object.
#' @param formula Formula to be evaluated.
#' @param data `data.frame` for the formula.
#' @param merge if `merge = TRUE` and `x` has multiple `numeric`
#'              columns, the result indices are merged.
#' @param which.i If `TRUE`, returns indices.
#' @param on Reducing period. If missing, period will be set to second smallest
#'           period in the series.
#' @param ... Unused
#' @return A list of indices/values of `xts`/`zoo`/`data.frame`
#'         objects. If `merge = TRUE`, values in the list are merged.
#'
#' @export
reduce <- function(x, ..., which.i = FALSE) UseMethod("reduce")

#' @describeIn reduce S3 method for `zoo` object
#' @export
reduce.zoo <- function(x, merge = TRUE, which.i = FALSE, on, ...) {
  index_xts <- utils::getFromNamespace("index.xts", "xts")
  i <- reduce_index(zoo::coredata(x), as.POSIXct(index_xts(x)), merge, on)
  if (which.i) return(i)
  if (merge) return(x[i])
  z <- lapply(seq_len(length(i)), function(j) x[i[[j]], j])
  names(z) <- names(x)
  return(z)
}

#' @describeIn reduce S3 method for `data.frame`
#' @export
reduce.data.frame <- function(x, merge = TRUE, which.i = FALSE, on, ...) {
  ti <- sapply(x, function(x) inherits(x, "POSIXt") | inherits(x, "Date"))
  if (sum(ti) != 1) stop("x must have only a 'POSIXt' based index column")
  di <- sapply(x, inherits, "numeric")
  if (sum(di) < 1) stop("x must have at least one 'numeric' column")
  fi <- sapply(x, inherits, "factor")
  if (any(fi)) {
    i <- reduce_long_df(x[, ti | di], x[, fi, drop = FALSE], on)
    if (which.i) return(i)
    return(x[i, ])
  }
  #
  i <- reduce_index(x[, di, drop = FALSE], x[, ti], merge, on)
  if (which.i) return(i)
  if (merge) return(x[i, ])
  data <- x[, ti | di, drop = FALSE]
  ti <- which(ti); di <- which(di)
  z <- lapply(seq_len(length(i)), function(j) data[i[[j]], c(ti, di[j])])
  names(z) <- names(x)[di]
  return(z)
}

#' @describeIn reduce S3 method for `formula`
#' @export
reduce.formula <- function(formula, data, which.i = FALSE, on, ...) {
  ti <- sapply(data, inherits, "POSIXt")
  if (sum(ti) != 1) stop("x must have only a 'POSIXt' based index column")
  a <- eval_formula(formula, data, na.action = stats::na.pass)
  a$x[, 1] <- as.POSIXct(a$x[, 1], attr(data[, 1], "tzone"),
                         origin = "1970-01-01")
  i <- reduce_long_df(a$x, a$by, on)
  if (which.i) return(i)
  return(data[i, ])
}

# x must be matrix-like object
# index is POSIXt vector
# merge is TRUE or FALSE
reduce_index <- function(x, index, merge, on) {
  if (missing(on)) {
    ep_max <- length(xts::endpoints(index, on = "seconds")) - 1
    ep_limit <- getOption("reduce_limit", ep_max)
    for (p in c("minutes", "hours", "days",
                "weeks", "months", "quarters", "years")) {
      ep <- xts::endpoints(index, on = p)
      if (length(ep) - 1 < ep_limit) break;
    }
  } else {
    ep <- endp(index, on)
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

reduce_long_df <- function(data, by, ...) {
  j <- split(seq_len(nrow(data)), by, lex.order = TRUE)
  i <- lapply(j, function(i) {
    d <- data[i,]
    if (any(dim(d) == 0)) return(NULL)
    reduce_index(d[, 2, drop = FALSE], d[, 1], TRUE, ...)
  })
  if (length(by) > 1) i[sapply(i, is.null)] <- NULL
  i <- mapply(function(x, y) y[x], i, j)
  if (is.list(i)) i <- do.call(c, i)
  return(as.vector(i))
}

endp <- function(x, on = "months", k = 1) {
  if (is.character(on)) {
    xts::endpoints(x, on, k)
  } else if (is.numeric(on)) {
    n <- NROW(x)
    n_pieces <- round(n/on[1])
    i <- if (n_pieces <= 1) list(seq_len(n)) else
      split(seq_len(n), cut(seq_len(n), n_pieces))
    names(i) <- NULL
    c(0, sapply(i, function(i) max(i)))
  }
}

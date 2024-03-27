#' Convert time-series object to a `tsarray` object
#'
#' Convert time-series object to a `tsarray` object by longest frequency of
#' time-series object.
#'
#' @param x a [ts], [zoo::zoo()]  or [xts] object
#' @param f Frequency (an integer value) or convert to matrix criteria for
#'          the object. Possible values are `(\%Y, \%m, \%d, \%H, \%M)`
#'          for year, month, day, hour and minute, respectively.
#' @return A `[tsarray]` object
#'
#' @export
as.tsarray <- function(x, ...) UseMethod("as.tsarray")

#' @describeIn as.tsarray S3 method for ts object
#' @keywords internal
#' @export
as.tsarray.ts <- function(x, ...) {
  a <- attributes(x)
  dn <- rev(dimnames(preformat(stats::time(x))))
  m <- if (inherits(x, "mts")) {
    cn <- colnames(x)
    dims <- c(length(dn[[2]]), length(dn[[1]]), length(cn))
    y <- lapply(1:ncol(x), function(i) preformat(x[,i]))
    array(as.numeric(unlist(y)), dims, c(dn[2], dn[1], list(cn)))
  } else preformat(x, TRUE)
  tsp <- as.numeric(colnames(m))
  tsp <- seq(min(tsp), by = 1/nrow(m), length.out = length(m))
  attr(m, "tsp2") <- c(min(tsp), max(tsp), nrow(m))
  class(m) <- "tsarray"
  attr(m, "from") <- a
  attr(m, "tm") <- tsp
  return(m)
}

get_longer_freq <- function(x) {
  fmt <- c(years = "%Y", months = "%m", days = "%d", hours = "%H",
           minutes = "%M")
  for (i in seq_along(fmt)) {
    if ((length(xts::endpoints(x, on = names(fmt[i]))) - 1) > 1)
      return(fmt[i])
  }
}

not_eq_parts <- function(x) {
  x <- as.character(x)
  y <- which(sapply(seq_len(nchar(x[1])), function(i) {
    i <- substr(x, i, i)
    any(i != i[1])
  }))
  substr(x, y[1], y[length(y)])
}

reduce_join <- function(t, f) {
  t <- as.POSIXlt(t)
  idx <- format(t, f)
  y1 <- tapply(seq_along(idx), idx, identity)
  y2 <- lapply(y1, function(i) {
    ti <- t[i]
    if (f == "%Y") ti$year <- rep_len(4, length(ti))
    if (f == "%m") ti$mon <- rep_len(0, length(ti))
    if (f == "%d") ti$mday <- rep_len(1, length(ti))
    if (f == "%H") ti$hour <- rep_len(0, length(ti))
    if (f == "%M") ti$min <- rep_len(0, length(ti))
    ti
  })
  y <- mapply(function(x, y, cn) {
    df <- list(x, y)
    names(df) <- c("rn", cn)
    df
  }, y2, y1, unique(idx), SIMPLIFY = FALSE)
  merge_df <- utils::getFromNamespace("merge.data.frame", "base")
  df <- Reduce(function(...) merge_df(..., by = "rn", all = TRUE), y)
  colnames(df)[-1] <- unique(idx)
  df <- df[order(df$rn),]
  df$rn <- not_eq_parts(format(df$rn, usetz = TRUE))
  df
}

as_tsarray <- function(x, f) {
  t <- stats::time(x)
  im <- reduce_join(t, f)
  tm <- as.data.frame(lapply(im[,-1], function(i) t[i]))
  colnames(tm) <- colnames(im)[-1]
  a <- attributes(x)
  x <- zoo::coredata(x)
  m <- if (!is.null(dim(x)) && ncol(x) > 1) {
    aperm(sapply(im[,-1], function(i) x[i,], simplify = "array"),
          c(1, 3, 2))
  } else {
    sapply(im[,-1], function(i) x[i])
  }
  if (f == "%Y" | f == "%m") {
    if (nrow(m) == 12)
      rownames(m) <- month.abb
    else if (ncol(m) == 12)
      colnames(m) <- month.abb
    else rownames(m) <- im$rn
  } else rownames(m) <- im$rn
  class(m) <- "tsarray"
  attr(m, "from") <- a
  attr(m, "tm") <- tm
  return(m)
}

#' @describeIn as.tsarray S3 method for zoo object
#' @keywords internal
#' @export
as.tsarray.zoo <- function(x, ...) {
  f <- utils::getFromNamespace("frequency.zoo", "zoo")(x)
  if (f <= 1) f <- get_longer_freq(x)
  if (is.character(f)) return(as_tsarray(x, f))
  if (!is.null(dim(x)) && ncol(x) > 1 && is.numeric(f)) {
    t <- as.numeric(stats::time(x))
    y <- stats::ts(zoo::coredata(x), frequency = stats::frequency(x))
    stats::tsp(y) <- c(min(t), max(t), stats::frequency(x))
    return(as.tsarray(y))
  }
  t <- as.numeric(stats::time(x))
  y <- stats::ts(zoo::coredata(x), frequency = stats::frequency(x))
  stats::tsp(y) <- c(min(t), max(t), stats::frequency(x))
  m <- as.tsarray(y)
  from <- attr(m, "from")
  if (!is.null(from$dimnames)) {
    from$dimnames <- dimnames(x)
    attr(m, "from") <- from
  }
  return(m)
}

#' @keywords internal
#' @export
as.xts.tsarray <- function(x, ...) {
  from <- attr(x, "from")
  if (!is.null(from$class)) {
    if ("ts" %in% from$class) {
      y <- xts::as.xts(stats::as.ts(x))
      # attr(y, "scaled:center") <- attr(x, "scaled:center")
      # attr(y, "scaled:scale") <- attr(x, "scaled:scale")
      # attr(y, "scaled:multiple") <- attr(x, "scaled:multiple")
      return(y)
    }
    if ("xts" %in% from$class) return(as.zoo.tsarray(x))
  } else stop("tsarray-matrix object must be created from an xts object")
}

as.zoo.tsarray <- function(x) {
  from <- attr(x, "from")
  if (!is.null(from$class)) {
    if ("ts" %in% from$class) {
      y <- zoo::as.zoo(stats::as.ts(x))
      attr(y, "frequency") <- NULL
      class(y) <- "zoo"
      # for (a in c("scaled", "scaled2"))
      #   for (b in paste0(a, ":", c("center", "scale", "multiple")))
      #     attr(y, b) <- attr(x, b)
      return(y)
    }
    if ("zoo" %in% from$class) {
      tm <- attr(x, "tm")
      y <- if (!is.null(tm)) {
        matrix2xts <- function(x) {
          do.call(rbind, lapply(seq_len(ncol(x)), function(i) {
            v <- as.vector(x[,i]); idx <- tm[,i]
            i <- !is.na(idx)
            xts::xts(v[i], order.by = idx[i])
          }))
        }
        if (length(dim(x)) == 3) {
          y <- apply(x, 3, function(x) list(matrix2xts(x)))
          do.call(xts::merge.xts, lapply(y, function(x) x[[1]]))
        } else matrix2xts(x)
      } else {
        stop(class(x)[1], " object must have a tm attribute")
      }
      attributes(y) <- from
      # for (a in c("scaled", "scaled2"))
      #   for (b in paste0(a, ":", c("center", "scale", "multiple")))
      #     attr(y, b) <- attr(x, b)
      return(y)
    }
  }
  stop("Cannot convert to zoo/xts object")
}

#' @export
#' @keywords internal
#' @importFrom stats as.ts
as.ts.tsarray <- function(x, ...) {
  from <- attr(x, "from")
  if (!is.null(from$class)) {
    t <- attr(x, "tsp2")
    tsp <- seq(t[1], t[2], 1/t[3])
    t2 <- from$tsp
    tsp2 <- seq(t2[1], t2[2], 1/t2[3])
    t <- if (length(tsp) == length(tsp2) && all(tsp == tsp2)) seq_len(length(tsp)) else
      unlist(sapply(tsp2, function(t) which(abs(t - tsp) <= 1e-7)))
    y <- if ("mts" %in% from$class) {
      array(x, dim = c(dim(x)[1] * dim(x)[2], dim(x)[3]))[t,]
    } else if ("ts" %in% from$class) {
      x[t]
    }
    attributes(y) <- from
    # for (a in c("scaled", "scaled2"))
    #   for (b in paste0(a, ":", c("center", "scale", "multiple")))
    #     attr(y, b) <- attr(x, b)
    return(y)
  } else stop("tsarray-matrix object must be created from an ts object")
}

#' @export
#' @keywords internal
as.matrix.tsarray <- function(x, ...) {
  if (is.matrix(x)) {
    attr(x, "tsp2") <- attr(x, "tm") <- attr(x, "from") <- NULL
    class(x) <- NULL
  } else if (is.array(x)) {
    from <- attr(x, "from")
    if (!is.null(from)) {
      x <- if ("xts" %in% from$class) {
        as.matrix(zoo::as.zoo(x))
      } else if ("zoo" %in% from$class) {
        as.matrix(xts::as.xts(x))
      } else if ("ts" %in% from$class) {
        from <- attr(x, "from"); f <- from$tsp[3]
        t(preformat(stats::as.ts(x), (f == 4 | f == 12)))
      }
    }
  }
  NextMethod()
}

#' @export
#' @keywords internal
print.tsarray <- function(x, ...) {
  if (is.array(x)) {
    x <- unclass(x)
    attr(x, "tsp2") <- attr(x, "tm") <- attr(x, "from") <- NULL
    NextMethod()
  } else stop("tsarray object must inherit 'array'")
}

preformat <- function(x, calendar = TRUE, ...) {
  fr.x <- stats::frequency(x)
  Tsp <- stats::tsp(x)
  if (is.null(Tsp))
    stop("series is corrupt, with no 'tsp' attribute")
  nn <- 1 + round((Tsp[2L] - Tsp[1L]) * Tsp[3L])
  if (NROW(x) != nn) {
    warning(gettextf("series is corrupt: length %d with 'tsp' implying %d",
                     NROW(x), nn), domain = NA, call. = FALSE)
    calendar <- FALSE
  }
  if (NCOL(x) == 1) {
    if (calendar) {
      if (fr.x > 1) {
        dn2 <- if (fr.x == 12) {
          month.abb
        } else if (fr.x == 4) {
          c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
        } else paste0("p", 1L:fr.x)
        if (NROW(x) <= fr.x && stats::start(x)[1L] == stats::end(x)[1L]) {
          dn1 <- stats::start(x)[1L]
          dn2 <- dn2[1 + (stats::start(x)[2L] - 2 + seq_along(x)) %% fr.x]
          x <- matrix(format(x, ...), nrow = 1L, byrow = TRUE,
                      dimnames = list(dn1, dn2))
        } else {
          start.pad <- stats::start(x)[2L] - 1
          end.pad <- fr.x - stats::end(x)[2L]
          dn1 <- stats::start(x)[1L]:stats::end(x)[1L]
          x <- matrix(c(rep.int("", start.pad), format(x, ...),
                        rep.int("", end.pad)),
                      ncol = fr.x, byrow = TRUE, dimnames = list(dn1, dn2))
        }
      } else {
        tx <- stats::time(x)
        attributes(x) <- NULL
        names(x) <- tx
      }
    } else {
      attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
  } else {
    rownames(x) <- if (calendar && fr.x > 1) {
      tm <- stats::time(x)
      t2 <- 1 + round(fr.x * ((tm + 0.001) %% 1))
      p1 <- format(floor(zapsmall(tm, digits = 7)))
      if (fr.x == 12)
        paste(month.abb[t2], p1)
      else paste(p1, if (fr.x == 4)
        c("Q1", "Q2", "Q3", "Q4")[t2]
        else format(t2))
    }
    else format(stats::time(x))
    attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
  }
  if (is.character(x)) {
    x <- trimws(x)
    dn <- dimnames(x)
    x[x == "" | x == "NA"] <- NA
    x <- apply(x, 2, as.numeric)
    dimnames(x) <- dn
  }
  t(x)
}

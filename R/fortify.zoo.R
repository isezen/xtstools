get_names <- function(names, nm = c("Index", "Series", "Value")) {
  if (!is.null(names(names))) names <- names[nm]
  if (is.list(names)) {
    names(names) <- nm
    for (i in 1L:3L) if (is.null(names[[i]]) || anyNA(names[[i]]))
      names[[i]] <- nm[i]
    nm <- unlist(names)
  } else {
    names <- c(names, rep(NA, length(nm) - length(names)))
    nm[!is.na(names)] <- names[!is.na(names)]
  }
  return(nm)
}

get_by_factor <- function(index, by) {
  if (!is.null(by) && length(by) > 0) {
    index <- as.POSIXlt(index)
    if (inherits(by, "formula")) by <- all.vars(by)
    if (is.character(by)) {
      choices <- c(Y = "year", m = "mon", d = "mday", H = "hour", M = "min",
                   S = "sec", MDH = "mdh", WeekDay = "wday",
                   WeekDayHour = "wdayhour")
      by <- sapply(by, function(x) if (x %in% names(choices)) choices[x] else x)
      names(choices) <- c("Year", "Month", "Day", "Hour", "Min", "Sec", "MDH",
                          "WeekDay", "WeekDayHour")
      by <- sapply(by, function(x) if (x %in% names(choices)) choices[x] else x)
      by <- tryCatch({match.arg(by, choices, several.ok = TRUE)},
                     error = function(e){NULL})
      if (is.null(by)) return(list())
      i <- unclass(index)
      f <- lapply(by, function(p) {
        # i <- i[[p]] + if (p == "year") 1900 else if (p == "mon") 1 else 0
        i <- indexx(index, p)
        if (p == "mon") {
          ui <- unique(i)
          factor(i, levels = ui, labels = month.name[ui])
        }
        else
          factor(i)
      })
      for (p in by) {
        i[[p]] <- rep_len(if (p == "mday") 1 else 0, length(i[[p]]))
      }
      class(i) <- c("POSIXlt", "POSIXt")
      return(list(index = i, by = f))
    }
  }
  list()
}

#' Convenience Function for Plotting xts or zoo Objects with ggplot2
#'
#' \code{fortify.zoo} takes a zoo object and converts it into a data frame
#' (intended for ggplot2).
#'
#' @param model An object of class \code{zoo} to be converted to a
#'              \code{data.frame}.
#' @param data,... Not used.
#' @param names (list of) character vector(s). New names given to index/time
#'              column, series indicator (if melted), and value column
#'              (if melted). If only a subset of characters should be changed,
#'              either NAs can be used or a named vector.
#' @param melt If \code{TRUE}, resulting \code{data.frame} is in long format.
#' @param sep If specified then the Series column is split into multiple
#'            columns using sep as the split character.
#' @param by bla bla bla
#' @param reduce If \code{TRUE}, temporal resolution of the object is reduced.
#'               This is useful especially for large \code{xts} objects.
#'               See \code{\link{reduce}}.
#' @keywords internal
fortify.zoo <- function(model, data, names = c("Index", "Series", "Value"),
                        melt = FALSE, sep = NULL, by = NULL, reduce = FALSE,
                        reformat.Index = TRUE, ...) {
  if (!is.null(sep) && !melt) stop("Cannot specify sep if melt = FALSE")
  n <- NROW(model)
  k <- NCOL(model)
  dots <- list(...)
  dots <- dots[names(dots) %in% c("row.names", "check.rows",
                                  "check.names", "fix.empty.names",
                                  "stringsAsFactors")]
  model <- check_object_colnames(model)
  lab <- colnames(model)
  if (is.null(lab)) lab <- rep.int(deparse(substitute(model)), k)
  lab <- make.unique(lab)
  nm <- get_names(names)
  index_xts <- utils::getFromNamespace("index.xts", "xts")
  index <- index_xts(model)
  if (melt) {
    by <- get_by_factor(index, by)
    if (length(by) > 0) {
      if (reformat.Index) index <- by$index
      by <- by$by
    }
    df <- if (k == 1L) {
      do.call("data.frame",
              c(list(index, factor(rep.int(1, n), labels = lab)),
                by, list(zoo::coredata(model)), dots))
    } else {
      do.call("data.frame",
              c(list(index[rep.int(1:n, k)],
                     factor(rep(1:k, each = n), levels = 1:k, labels = lab)),
                by, list(as.vector(zoo::coredata(model))), dots))
    }
    if (reduce) {
      i <- reduce.zoo(model, FALSE, TRUE)
      df <- df[do.call(c, mapply(`+`, i, cumsum(c(0, rep(n, k - 1))))), ]
    }
    # browser()
    ns <- 1
    if (!is.null(sep)) {
      sep <- if (is.character(sep)) sep[1] else "."
      spl <- strsplit(as.character(df[[2L]]), sep, fixed = TRUE)
      ns <- len(spl[[1]])
      df <- data.frame(df[1L], do.call("rbind", spl), df[3:ncol(df)])
    }
    nl <- length(nm)
    # un <- make.unique(rep_len(nm[2:(ns + 1)], ns), sep = "")
    if (ns > 1) {
      un <- make.unique(rep_len(nm[-c(1L, nl)], ncol(df) - 1L), sep = "")
      names(df) <- c(nm[1L], un[-1L], nm[nl])
    } else {
      un <- make.unique(rep_len(nm[-c(1L, nl)], ncol(df) - 2L), sep = "")
      names(df) <- c(nm[1L], un, nm[nl])
    }
    # names(df)[c(1, 2:(ns + 1), ncol(df))] <- c(nm[1L], un, nm[nl])
  } else {
    if (reduce) {
      i <- reduce.zoo(model, TRUE, TRUE)
      model <- model[i]
      index <- index_xts(model)
    }
    by <- get_by_factor(index, by)
    if (length(by) > 0) {
      index <- by$index
      by <- by$by
    }
    df <- cbind(do.call("data.frame", c(list(index), by, dots)),
                zoo::coredata(model))
    names(df)[c(1, (ncol(df) - length(lab) + 1):ncol(df))] <- c(nm[1L], lab)
  }
  return(df)
}

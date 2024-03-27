#' indexx function
#'
#' Extreact various date properties from `POSIXt`, `xts` or `zoo`
#' objects.
#'
#' @param x `POSIXt`, `xts` or `zoo` object.
#' @param unit Unit as character vector. Possible values are `sec, min,
#' hour, mday, mon, year, wday, yday, isdst` and `yearmon, wdayhour, md,
#' mdh, yhour, index`.
#'
#' @export
#' @keywords internal
indexx <- function(x, unit) UseMethod("indexx")

#' @export
#' @keywords internal
indexx.POSIXlt <- function(x, unit) {
  base_units <-  c("sec", "min", "hour", "mday", "mon", "year", "wday", "yday",
                   "isdst")
  unit <- match.arg(unit, c("yearmon", "wdayhour", "md", "mdh", "yhour",
                            "index", base_units))
  if (unit == "yearmon") {
    indexx_internal(x, "year") * 1000 + indexx_internal(x, "mon")
  } else if (unit == "wdayhour") {
    indexx_internal(x, "wday") * 100 + indexx_internal(x, "hour")
  } else if (unit == "md") {
    indexx_internal(x, "mon") * 100 + indexx_internal(x, "mday")
  } else if (unit == "mdh") {
    indexx_internal(x, "mon") * 10000 + indexx_internal(x, "mday") *
      100 + indexx_internal(x, "hour")
  }else if (unit == "yhour") {
    indexx_internal(x, "yday") * 100 + indexx_internal(x, "hour")
  } else if (unit == "index") {
    as.numeric(x)
  } else if (unit %in% base_units) {
    indexx_internal(x, unit)
  }
}

#' @export
#' @keywords internal
indexx.POSIXct <- function(x, unit) {
  indexx.POSIXlt(as.POSIXlt(x, tz = xts::tzone(x)), unit)
}

#' @export
#' @keywords internal
indexx.Date <- function(x, unit) {
  indexx.POSIXlt(as.POSIXlt(x, tz = xts::tzone(x)), unit)
}

#' @export
#' @keywords internal
indexx.zoo <- function(x, unit) indexx(index.zoo(x), unit)

#' @export
#' @keywords internal
indexx.xts <- function(x, unit) indexx(index.xts(x), unit)

index.zoo <- function(x, ...)
  utils::getFromNamespace("index.zoo", "zoo")(x, ...)

index.xts <- function(x, ...)
  utils::getFromNamespace("index.xts", "xts")(x, ...)

indexx_internal <- function(x, unit) {
  r <- unclass(x)[[unit]]
  if (unit == "year") r <- r + 1900
  if (unit  == "wday") r[which(r == 0)] <- 7
  if (unit %in% c("mon", "yday")) r <- r + 1
  return(r)
}

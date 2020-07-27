library(testthat)
library(xtstools)

test_ts <- function(l, f, start = 1 + 1/f, add_missing = TRUE,
                    multicol = FALSE) {
  get_r <- function(x) {
    calendar <- (f == 4 | f == 12)
    if (multicol) {
      r <- stats:::.preformat.ts(x, calendar)
    } else {
      r <- trimws(stats:::.preformat.ts(x, TRUE)); r[r == "" | r == "NA"] <- NA
      r <- t(apply(r, 2, as.numeric)); colnames(r) <- tsp(x)[1]:tsp(x)[2]
    }
    return(r)
  }
  i <- ifelse(add_missing, ceiling(l * 0.3), 0)
  data <- (1 + i):(l - i)
  if (multicol) data <- matrix(rep(data, 5), ncol = 5,
                               dimnames = list(NULL, letters[1:5]))
  if (add_missing) {
    set.seed(1)
    data[sample.int(length(data), 5)] <- NA
  }
  indices <- seq(1, by = 1/f, length.out = l)
  indices <- indices[(1 + i):(l - i)]
  x <- ts(data, start = indices[1], frequency = f)
  y <- as.tsarray(x)
  expect_equal(as.matrix(y), get_r(x))
  expect_equal(as.ts(y), x)
  invisible(y)
}

context("tsarray ts")
for (am in c(TRUE, FALSE))
  for (mc in c(TRUE, FALSE))
    for (f in c(4, 6, 12))
      for (s in c(1, 1 + 1/f)) {
        fmt <- "ts: freq = %d, start =%f, multicol = %d, missing = %d"
        test_that(sprintf(fmt, f, s, mc, am),
                  {test_ts(120, f, s, am, mc)})
      }

create_zoo <- function(l, by, f = NULL, as = "zoo", multicol = FALSE,
                       make_matrix = TRUE, add_missing = TRUE) {
  const <- if (as == "xts") xts::xts else zoo::zoo
  i <- ifelse(add_missing, ceiling(l * 0.3), 0)
  data <- (1 + i):(l - i)
  if (multicol) data <- matrix(rep(data, 5), ncol = 5, dimnames = list(NULL, letters[1:5]))
  if (add_missing) {
    set.seed(1)
    data[sample.int(length(data), 5)] <- NA
  }
  if (!multicol && make_matrix) data <- matrix(data, dimnames = list(NULL, "a"))
  ob <- seq(ISOdate(2020, 1, 1, 0), by = by, length.out = l)
  x <- const(data, ob[(1 + i):(l - i)], frequency = f)
  x
}

test_zoo_xts <- function(as = "zoo", multicol = FALSE, make_matrix = TRUE,
                         add_missing = TRUE) {
  as.fun <- if (as == "zoo") zoo::as.zoo else xts::as.xts
  create <- function(l, by, f = NULL) {
    create_zoo(l, by, f, as, multicol, make_matrix, add_missing)
  }
  # test for different type of indices
  x <- create(12 * 3, "month")
  y1 <- as.tsarray(x)
  expect_equal(as.fun(y1), x)
  zoo::index(x) <- zoo::as.yearmon(zoo::index(x))
  y2 <- as.tsarray(x)
  expect_equal(as.fun(y2), x)
  expect_equal(y1, y2, check.attributes = FALSE)

  x <- create(12 * 3, "3 month")
  y1 <- as.tsarray(x)
  expect_equal(as.fun(y1), x)
  zoo::index(x) <- zoo::as.yearqtr(zoo::index(x))
  y2 <- as.tsarray(x)
  expect_equal(as.fun(y2), x)
  expect_equal(y1, y2, check.attributes = FALSE)

  x <- create(12 * 10, "day")
  zoo::index(x) <- as.Date(zoo::index(x))
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)

  # test for year - 2
  x <- create(365 * 5, "day")
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)

  # test for year - 3
  x <- create(24 * 365 * 2, "hour")
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)

  # test for month
  x <- create(30 * 12, "day")
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)

  # test for day
  x <- create(24 * 5, "hour")
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)

  # test for hour
  x <- create(60 * 5, "min")
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)

  # test for minute
  x <- create(60 * 10, "sec")
  y <- as.tsarray(x)
  expect_equal(as.fun(y), x)
}

for (as in c("zoo", "xts")) {
  context(paste("tsarray", as))
  for (mc in c(TRUE, FALSE))
    for (mm in c(TRUE, FALSE))
      for (am in c(TRUE, FALSE)) {
        fmt <- "%s: multicol = %d, matrix = %d, missing = %d"
        test_that(sprintf(fmt, as, mc, mm, am),
                  {test_zoo_xts(as, mc, mm, am)})
      }
}


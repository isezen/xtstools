library(testthat)
context("indexx")

by <-  c("sec", "min", "hour", "day", "month", "year", "week")
indexx <- getFromNamespace("indexx", "xtstools")
a <- lapply(seq_along(by), function(i) seq(ISOdate(2020,1,1), by = by[i],
                                           length.out = 5))
names(a) <- by

test_that("Base Units", {
  expect_equal(indexx(a$sec, "sec"), 0:4)
  expect_equal(indexx(a$min, "min"), 0:4)
  expect_equal(indexx(a$hour, "hour"), 12:16)
  expect_equal(indexx(a$day, "mday"), 1:5)
  expect_equal(indexx(a$month, "mon"), 1:5)
  expect_equal(indexx(a$year, "year"), 2020:2024)
  expect_equal(indexx(a$year, "wday"), c(3, 5:7, 1))
  expect_equal(indexx(a$week, "wday"), rep(3, 5))
  expect_equal(indexx(a$year, "yday"), rep(1, 5))
  expect_equal(indexx(a$month, "yday"), c(1, 32, 61, 92, 122))
})

test_that("Extended Units", {
  expect_equal(indexx(a$month, "yearmon"), 2020001:2020005)
  expect_equal(indexx(a$hour, "wdayhour"), 312:316)
  expect_equal(indexx(a$day, "md"), 101:105)
  expect_equal(indexx(a$hour, "mdh"), 10112:10116)
  expect_equal(indexx(a$hour, "yhour"), 112:116)
  expect_equal(indexx(a$hour, "index"), seq(1577880000, by = 3600,
                                            length.out = 5))
  expect_error(indexx(a$sec, "asdf"))
})

test_that("xts objects", {
  order.by = seq(ISOdate(2020, 1, 1), by = "sec", length.out = 5)
  x <- rnorm(5)
  expect_equal(indexx(xts::xts(x, order.by), "sec"), 0:4)
  expect_equal(indexx(zoo::zoo(x, order.by), "sec"), 0:4)
})

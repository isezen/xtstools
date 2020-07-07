library(testthat)
context("boxcox")

loglik <- utils::getFromNamespace("loglik", "xtstools")
boxcox_internal <- utils::getFromNamespace("boxcox_internal", "xtstools")

set.seed(1)
x <- rnorm(10)
x <- x + abs(min(x)) + 1

test_that("boxcox_internal", {
  expect_equal(boxcox_internal(x, 0), log(x))
  expect_equal(boxcox_internal(x, 0.5), (x^0.5 - 1)/0.5)
  expect_equal(boxcox_internal(boxcox_internal(x, 0), 0, inverse = TRUE), x)
  expect_equal(boxcox_internal(boxcox_internal(x, 0.5), 0.5, inverse = TRUE), x)
})

test_that("boxcox.lambda", {
  expect_equal(boxcox.lambda(x, 2), 0.3845291444)
  x[1] <- -1
  expect_warning(
    boxcox.lambda(x),
    paste0("Any zero or negative value (which x contains) will ",
           "be set to NA for the sake of transformation"), fixed = TRUE)
})

set.seed(1234)
x <- rexp(1000, 1)

test_that("boxcox.plus", {
  mm <- function(x, logf = log) abs(mean(logf(x)) - median(logf(x)))
  i <- boxcox.plus(x)
  expect_equal(loglik(x + i, 0) > loglik(x, 0), TRUE)
  expect_equal(mm(x + i) < mm(x), TRUE)
  expect_equal(i, 0.08319605065)
  expect_equal(boxcox.lambda(x + i), 0.1042982462)
})

test_that("boxcox", {
  y <- boxcox(x)
  expect_equal(mean(y), -0.35941546)
  expect_equal(median(y), -0.3176390584)
  expect_equal(attr(y, "lambda"), 0.2760974371)
  expect_equal(boxcox(y, inverse = TRUE), x)
  x[1] <- -1
  expect_warning(
    boxcox(x),
    paste0("Any zero or negative value (which x contains) will ",
           "be set to NA for the sake of transformation"), fixed = TRUE)
})

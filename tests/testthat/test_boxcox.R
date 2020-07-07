library(testthat)
context("boxcox")

loglik <- utils::getFromNamespace("loglik", "xtstools")
boxcox_internal <- utils::getFromNamespace("boxcox_internal", "xtstools")

test_that("boxcox_internal", {
  x <- rnorm(10)
  x <- x + abs(min(x)) + 1
  expect_equal(boxcox_internal(x, 0), log(x))
  expect_equal(boxcox_internal(x, 0, log_10 = TRUE), log10(x))
  expect_equal(boxcox_internal(x, 0.5), (x^0.5 - 1)/0.5)
  expect_equal(boxcox_internal(boxcox_internal(x, 0), 0, inverse = TRUE), x)
  expect_equal(boxcox_internal(boxcox_internal(x, 0, log_10 = TRUE), 0,
                               inverse = TRUE, log_10 = TRUE), x)
  expect_equal(boxcox_internal(boxcox_internal(x, 0.5), 0.5, inverse = TRUE), x)
})

test_that("boxcox.lambda", {
  set.seed(1)
  x <- rlnorm(10)
  expect_equal(boxcox.lambda(x, 2), -0.2029405125)
  expect_equal(boxcox.lambda(x, 2, log_10 = TRUE), -0.2029405335)
  x[1] <- -1
  expect_warning(
    boxcox.lambda(x),
    paste0("Any zero or negative value (which x contains) will ",
           "be set to NA for the sake of transformation"), fixed = TRUE)
})

test_that("boxcox.plus", {
  mm <- function(x, logf = log) abs(mean(logf(x)) - median(logf(x)))
  set.seed(1234)
  x <- rexp(1000, 1)
  i <- boxcox.plus(x)
  expect_equal(loglik(x + i, 0) > loglik(x, 0), TRUE)
  expect_equal(mm(x + i) < mm(x), TRUE)
  expect_equal(i, 0.08319605065)
  expect_equal(boxcox.lambda(x + i), 0.1042982462)
  i <- boxcox.plus(x, log_10 = TRUE)
  expect_equal(i, 0.08319606307)
  expect_equal(mm(x + i, log10) < mm(x, log10), TRUE)
  expect_equal(boxcox.lambda(x + i), 0.1042982278)
})

test_that("boxcox", {
  set.seed(1234)
  x <- rexp(1000, 1)
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

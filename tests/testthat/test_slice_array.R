library(testthat)

slice_array <- getFromNamespace("slice_array", "xtstools")

context("slice_array")
test_that("slice_array", {
  x <- matrix(1:20, 5, 4)
  expect_equal(slice_array(x, 2), x)
  expect_equal(slice_array(x, 1), x)

  x <- array(1:120, dim = c(6, 5, 4))
  expect_equal(slice_array(x, 2, 1), x[,1,])
  expect_equal(slice_array(x, 1, 2), x[2,,])
  expect_equal(slice_array(x, 1, 2:3), x[2:3,,])
  expect_equal(slice_array(x, 2, 2:3), x[,2:3,])
  expect_equal(slice_array(x, 3, 2:3), x[,,2:3])
})





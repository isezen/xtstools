library(testthat)
options(mc.cores = 1)

context("wapply")
test_that("wapply", {
  x <- matrix(1:20, 5, 4)
  wapply(x, 1, identity, step_size = 1, wsize = 3)

  x <- array(1:120, dim = c(6, 5, 4))
  wapply(x, 2, function(x) dim(x), step_size = 1, wsize = 3)

})





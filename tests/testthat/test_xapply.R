library(testthat)
options(mc.cores = 1)

context("xapply")
test_that("xapply", {
  x <- matrix(1:20, 5, 4)
  xapply(x, 1, identity, 1, 3, multiple = TRUE)

  x <- array(1:120, dim = c(6, 5, 4))
  xapply(x, 2:3, function(x) dim(x), 1, 3, multiple = TRUE)

})





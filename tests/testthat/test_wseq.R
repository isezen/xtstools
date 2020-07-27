library(testthat)
library(xtstools)

wseq <- getFromNamespace("wseq", "xtstools")
wseq_test <- function(x, i = 1:length(x), size = 1, clamped = TRUE) {
  w <- floor(size/2)
  r <- sapply(i, function(j) seq(j - w, j + w))
  if (clamped) {
    r <- r %% length(x)
    r[r == 0] <- length(x)
  } else {
    if (is.matrix(r)) {
      j <- which(apply(r, 2, function(j) any(j < 1)))
      for (k in j) r[,k] <- 1:nrow(r)
      j <- which(apply(r, 2, function(j) any(j > length(x))))
      for (k in j) r[,k] <- (length(x) - nrow(r) + 1):length(x)
    }
  }
  if (is.matrix(r)) matrix(x[r], nrow(r), ncol(r)) else x[r]
}

context("wseq")
test_that("wseq", {
  x <- 1:26
  for (s in seq(1, 9, 2))
    for (c in c(TRUE, FALSE))
      for (i in sapply(seq(1, length(x), 5), function(i) seq(1, i)))
        expect_equal(wseq(x, i, s, c), wseq_test(x, i, s, c))
})

wseq2 <- getFromNamespace("wseq2", "xtstools")
wseq2_test <- function(x, step_size = 1, wsize = 1, clamped = TRUE) {
  l <- length(x)
  middle <- ceiling(l/2)
  steps <- c(rev(seq(middle, 1, -step_size[1])[-1]),
             seq(middle, l, step_size[1]))
  i <- wseq_test(x, steps, size = wsize[1], clamped = clamped)
  if (is.matrix(i)) {
    step_size <- step_size[-1]
    wsize <- wsize[-1]
    if (length(wsize) > 0) {
      j <- apply(i, 2, function(y) {
        as.vector(wseq2_test(y, step_size, wsize, FALSE))
      })
      i <- j
    }
  }
  i
}

test_that("wseq2", {
  x <- letters
  step_size <- c(1, 4)
  wsize <- c(10, 3)
  clamped <- TRUE
  expect_equal(
    wseq2(x, step_size, wsize, clamped),
    wseq2_test(x, step_size, wsize, clamped))
})




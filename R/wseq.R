
wseq <- function(x, i = 1:length(x), size = 1, clamped = TRUE) {
  sapply(i - 1, function(i) wseq_cpp(x, i, as.integer(size), as.logical(clamped)))
}

wseq2 <- function(x, step_size = 1, wsize = 1, clamped = TRUE) {
  l <- length(x)
  middle <- ceiling(l/2)
  steps <- c(rev(seq(middle, 1, -step_size[1])[-1]),
             seq(middle, l, step_size[1]))
  i <- wseq(x, steps, size = wsize[1], clamped = clamped)
  if (is.matrix(i)) {
    step_size <- step_size[-1]
    wsize <- wsize[-1]
    if (length(wsize) > 0) {
      j <- apply(i, 2, function(y) {
        as.vector(wseq2(y, step_size, wsize, FALSE))
      })
      i <- j
    }
  }
  i
}

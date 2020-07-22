library(testthat)
context("mstl2")

test_that("mstl2", {
  x <- AirPassengers

  # additive
  s1 <- mstl2(x, s.window = "periodic")
  expect_equal(x, Reduce("+", as.list(s1[,2:4])))

  # multiplicative
  s2 <- mstl2(x, s.window = "periodic", type = "m")
  expect_equal(x, Reduce("*", as.list(s2[,2:4])))

  # scale by median and mad
  # ---------------------------------------------------------------------------
  x2 <- scalex::scalex(x)
  class(x2) <- "ts"
  attr(x2, "scaled:center") <- attr(x2, "scaled:scale") <- NULL
  x2 <- x2 + boxcox.plus(x2)

  # additive
  s4 <- mstl2(x2, s.window = "periodic")
  expect_equal(x2, Reduce("+", as.list(s4[,2:4])))

  # multiplicative
  s5 <- mstl2(x2, s.window = "periodic", type = "m")
  expect_equal(x2, Reduce("*", as.list(s5[,2:4])))

  # center the scale
  x3 <- scalex::scalex(x, center = TRUE)
  class(x3) <- "ts"
  attr(x3, "scaled:center") <- attr(x3, "scaled:scale") <- NULL
  attr(x3, "scaled2:center") <- NULL
  x3 <- x3 + abs(min(x3)) + 1

  # additive
  s6 <- mstl2(x3, s.window = "periodic")
  expect_equal(x3, Reduce("+", as.list(s6[,2:4])))

  # multiplicative
  s7 <- mstl2(x3, s.window = "periodic", type = "m")
  expect_equal(x3, Reduce("*", as.list(s7[,2:4])))

  # scale by mean and sd
  # ---------------------------------------------------------------------------
  x4 <- scalex::scalex2(x)
  class(x4) <- "ts"
  attr(x4, "scaled:center") <- attr(x4, "scaled:scale") <- NULL
  x4 <- x4 + boxcox.plus(x4)

  # additive
  s8 <- mstl2(x4, s.window = "periodic")
  expect_equal(x4, Reduce("+", as.list(s8[,2:4])))

  # multiplicative
  s9 <- mstl2(x4, s.window = "periodic", type = "m")
  expect_equal(x4, Reduce("*", as.list(s9[,2:4])))

  # ------------------------------
  # center the scale
  x5 <- scalex::scalex2(x, center = TRUE)
  class(x5) <- "ts"
  attr(x5, "scaled:center") <- attr(x5, "scaled:scale") <- NULL
  attr(x5, "scaled2:center") <- NULL
  x5 <- x5 + abs(min(x5)) + 1

  # additive
  s10 <- mstl2(ts(x5), s.window = "periodic")
  expect_equal(x5, Reduce("+", as.list(s10[,2:4])))

  # multiplicative
  s11 <- mstl2(ts(x5), s.window = "periodic", type = "m")
  expect_equal(x5, Reduce("*", as.list(s11[,2:4])))
})

test_that("mstl2_2", {
  nextodd <- function(x) {
    x <- round(x)
    if (x %% 2 == 0) x <- x + 1
    as.integer(x)
  }
  x <- zoo::na.approx(xtstools::na.medpol(scalex::adn[,2]))
  x <-  x + xtstools::boxcox.plus(x)
  xs <- forecast::msts(as.vector(x), seasonal.periods = c(24, 8766), start = 2008)

  # period <- frequency(xs)
  # n <- as.integer(length(xs))
  # s.window <- 10 * n + 1
  # t.window <- nextodd(ceiling(1.5 * period/(1 - 1.5/s.window)))

  ra <- zoo::rollapply(x, 8766, median, align = "center")
  s1 <- mstl2(xs, NULL, s.window = "periodic", type = "a")
  s2 <- mstl2(xs, NULL, s.window = "periodic", type = "m")

  f <- frequency(xs)
  filter <- if (!f %% 2) c(0.5, rep_len(1, f - 1), 0.5)/f else rep_len(1, f)/f
  filt <- filter(xs, filter)
  # s1 <- mstl2(xs, NULL, s.window = "periodic", type = "a", s.jump = 360, t.jump = 360, l.jump = 360)
  # s2 <- mstl2(xs, NULL, s.window = "periodic", type = "m", s.jump = 360, t.jump = 360, l.jump = 360)
  colnames(ra) <- "median"
  a <- cbind(ra, Trend.add = s1[,"Trend"], Trend.mul = s2[,"Trend"], filt = filt)
  ggxts::xplot(a, facets = NULL, main = "Original Series")

  # scaling
  x2 <- scalex::scalex_pm10(x, center = TRUE, scale = TRUE)
  # x2 <- x2["2013"]
  sc <- ts(attr(x2, "scaled:center"), frequency = 24)
  sca <- mstl2(sc, s.window = "periodic", type = "a")
  scm <- mstl2(sc, s.window = "periodic", type = "m")

  ss <- ts(attr(x2, "scaled:scale"), frequency = 24)
  ssa <- mstl2(ss, s.window = "periodic", type = "a")
  ssm <- mstl2(ss, s.window = "periodic", type = "m")

  x2 <-  x2 + boxcox.plus(x2)
  # ra2 <- zoo::rollapply(x2, 8766, median, align = "center")
  xs2 <- forecast::msts(as.vector(x2), seasonal.periods = c(24, 8766), start = 2008)
  s3 <- mstl2(xs2, NULL, s.window = "periodic", type = "a")
  s4 <- mstl2(xs2, NULL, s.window = "periodic", type = "m")

  x2r <- xts::xts(scalex::scalex(as.vector(x2)), order.by = zoo::index(x2))
  colnames(x2r) <- "x"
  a <- anomx::shesd(x2r, alpha = 1e-2, max_anoms = 0.05, only_anoms = FALSE,
                    alternative = "two.tail", piecewise.median.period = 52)
  b <- attr(a, "anomaly")
  # b <- b[,-(1:2)]
  medians <- attr(b, "medians")
  if (is.null(medians)) medians <- rep(0, length(x2))
  medians <- xts::xts(medians, order.by = zoo::index(x2))

  dx2r <- diff(x2r)
  dx2r[is.na(dx2r)] <- 0
  a2 <- anomx::shesd(dx2r, alpha = 1e-12, max_anoms = 1.0,
                     only_anoms = TRUE, alternative = "two.tail",
                     piecewise.median.period = 52)
  b2 <- attr(a2, "anomaly")
  b2_diff_index <- setdiff(b2$index, b$index)
  b <- rbind(b, b2[which(b2$index %in% b2_diff_index),])

  df <- xtstools:::fortify.zoo(x, by = Year ~ .)
  colnames(df)[3] <- "x"
  df2 <- df[b$index,]
  df2 <- cbind(df2, b)
  # df2$score <- log10(df2$score)
  df <- cbind(df, medians = medians)
  df$median_col <- as.factor(medians)
  df2$col <- cut(df2$score, quantile(df2$score, seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE)
  ggplot(df, aes(x = Index, y = x)) +
    geom_line() + geom_point(aes(x = Index, y = x, fill = col), df2, pch = 21, inherit.aes = FALSE) +
    geom_line(aes(y = medians, col = median_col), size = 1, show.legend = FALSE) +
    facet_grid(Year ~ ., scales = "free_y") +
    scale_fill_brewer(palette = "Spectral", direction = -1)






  a2 <- cbind(ra2, Trend.add = s3[,"Trend"], Trend.mul = s4[,"Trend"])
  ggxts::xplot(a2, facets = NULL, main = "Scaled Series")

  ggxts::xplot(x2)
  ggxts::xplot(x2 - as.vector(s3[,"Trend"]))
  ggxts::xplot(x2 / as.vector(s4[,"Trend"]))
  # ggxts::xplot(cbind(x, x - as.vector(s1[,"Trend"]), x / as.vector(s2[,"Trend"])))

  x5 <- x2 - as.vector(s3[,"Trend"])
  x4 <- x2 / as.vector(s4[,"Trend"])
  ra4 <- zoo::rollapply(x4, 8766, median, align = "center")
  xs4 <- forecast::msts(as.vector(x4), seasonal.periods = c(24, 8766), start = 2008)
  s41 <- mstl2(xs4, NULL, s.window = "periodic", type = "a")
  s42 <- mstl2(xs4, NULL, s.window = "periodic", type = "m")


  # scaling - 2
  x3 <- scalex::scalex_pm10(x, center = T)
  x3 <-  x3 + boxcox.plus(x3)
  ra3 <- zoo::rollapply(x3, 8766, median, align = "center")
  xs3 <- forecast::msts(as.vector(x3), seasonal.periods = c(24, 8766), start = 2008)
  s5 <- mstl2(xs3, NULL, s.window = "periodic", type = "a")
  s6 <- mstl2(xs3, NULL, s.window = "periodic", type = "m")

  a3 <- cbind(ra3, Trend.add = s5[,"Trend"], Trend.mul = s6[,"Trend"])
  ggxts::xplot(a3, facets = NULL, main = "Scaled and Centered Series")
})

test_that("mstl2_3", {
  library(xts);library(xtstools);library(ggplot2)
  x <- zoo::na.approx(xtstools::na.medpol(scalex::adn[,2]))
  x <-  x + xtstools::boxcox.plus(x)
  x2 <- scalex::scalex_pm10(x["2012/2014"], center = TRUE, scale = TRUE)
  x2 <-  x2 + xtstools::boxcox.plus(x2)
  xs2 <- forecast::msts(as.vector(x2), seasonal.periods = c(24, 8766), start = 2012)

  x2r <- xts::xts(scalex::scalex(as.vector(x2)), order.by = zoo::index(x2))
  colnames(x2r) <- "x"
  a <- anomx::shesd(x2r, alpha = 5e-2, max_anoms = 0.5, only_anoms = TRUE,
                    alternative = "two.tail", period = 8766)
  b <- attr(a, "anomaly")
  medians <- attr(b, "medians")
  if (is.null(medians)) medians <- rep(0, length(x2))
  medians <- xts::xts(medians, order.by = zoo::index(x2))

  df <- xtstools:::fortify.zoo(x, by = Year ~ .)
  colnames(df)[3] <- "x"
  df2 <- df[b$index,]
  df2 <- cbind(df2, b)
  # df2$score <- log10(df2$score)
  df <- cbind(df, medians = medians)
  df$median_col <- as.factor(medians)
  df2$col <- cut(df2$score, quantile(df2$score, seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE)
  ggplot(df, aes(x = Index, y = x)) +
    geom_line() + geom_point(aes(x = Index, y = x, fill = col), df2, pch = 21, inherit.aes = FALSE) +
    geom_line(aes(y = medians, col = median_col), size = 1, show.legend = FALSE) +
    facet_grid(Year ~ ., scales = "free_y") +
    scale_fill_brewer(palette = "Spectral", direction = -1)
})


test_that("mstl2_4", {
  nextodd <- function(x) {
    x <- round(x)
    if (x %% 2 == 0) x <- x + 1
    as.integer(x)
  }

  library(xts);library(xtstools);library(ggplot2)
  x <- zoo::na.approx(xtstools::na.medpol(scalex::adn[,2]))
  x <-  x + xtstools::boxcox.plus(x)
  x2 <- scalex::scalex_pm10(x, center = T, scale = T)
  x2 <-  x2 + xtstools::boxcox.plus(x2)
  xs2 <- forecast::msts(as.vector(x2), seasonal.periods = c(24), start = 2009)

  # period <- frequency(xs2)
  # n <- as.integer(length(xs2))
  # s.window <- 10 * n + 1
  # t.window <- nextodd(ceiling(1.5 * period/(1 - 1.5/s.window))) # 13151
  s <- list()
  s[["a"]] <- mstl2(xs2, NULL, s.window = "periodic", t.window = 8766 * 2,
               l.window = 25,
               s.jump = 168, t.jump = 168, l.jump = 168, type = "a", robust = FALSE)
  s[["m"]] <- mstl2(xs2, NULL, s.window = "periodic", t.window = 8766 * 2,
               l.window = 25,
               s.jump = 168, t.jump = 168, l.jump = 168, type = "m", robust = FALSE)
  s[["ar"]] <- mstl2(xs2, NULL, s.window = "periodic", t.window = 8766 * 2,
               l.window = 25,
               s.jump = 168, t.jump = 168, l.jump = 168, type = "a")
  s[["mr"]] <- mstl2(xs2, NULL, s.window = "periodic", t.window = 8766 * 2,
               l.window = 25,
               s.jump = 168, t.jump = 168, l.jump = 168, type = "m")

  sapply(s, attr, "err")

  # plot(s2a)
  st <- sapply(s, function(x) x[,"Trend"])
  st <- xts::xts(st, order.by = zoo::index(x))
  ggxts::xplot(st, facets = NULL)

  ss <- sapply(s, function(x) x[,"Seasonal24"])
  ss <- xts::xts(ss, order.by = zoo::index(x))
  ggxts::xplot(ss[1:48,], facets = NULL)

  sr <- sapply(s, function(x) x[,"Remainder"])
  sr <- xts::xts(sr, order.by = zoo::index(x))
  ggxts::xplot(sr[,c("m", "mr")], facets = Year ~ .)

  ls <- lapply(cbind(sr, xs2), function(x) {
    spectrum(as.vector(x), log = "yes", plot = TRUE)
  })
  ls2 <- lapply(ls, function(x) data.frame(spx = x$freq, spy = 2 * x$spec))
  op <- par(mfrow = c(3,2))
  for (i in seq_along(ls)) {
    n <- names(ls)[i]
    x <- ls[[i]]
    plot(x, xlab = "Frequency", ylab = "Spectral Density", main = n, type = "l")
  }
  par(op)
  spx <- ls2[[1]]$spx
  df <- sapply(ls2, function(x) x[,2])
  matplot(spx, log(df), type = "l")

  require(multitaper)
  rem <- ts(cbind(sr, xs2), frequency = 24)
  resSpec <- spec.mtm(rem[,5], nw = 24.0, k = 24 * 2, nFFT = "default",
                      Ftest = TRUE, jackknife = TRUE,
                      plot = TRUE, na.action = na.fail)

  trend <- numeric()
  time <- numeric()
  for (y in 2009:2018) {
    x2 <- scalex::scalex_pm10(x[as.character(y)], center = T, scale = T)
    x2 <-  x2 + xtstools::boxcox.plus(x2)
    xs2 <- forecast::msts(as.vector(x2), seasonal.periods = c(24), start = y)

    s2a <- mstl2(xs2, NULL, s.window = "periodic", t.window = 8766 * 2,
                 l.window = 25,
                 s.jump = 168, t.jump = 168, l.jump = 168, type = "a")
    # s2m <- mstl2(xs2, NULL, s.window = "periodic", t.window = 8766 * 2,
    #              l.window = 25,
    #              s.jump = 168, t.jump = 168, l.jump = 168, type = "m")
    trend <- c(trend, s2a[,"Trend"])
    time <- c(time, seq(y, y + 1, length.out = length( s2a[,"Trend"])))
  }
  plot(time, trend, type = "p")
  abline(v = 2009:2018)
})

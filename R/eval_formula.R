#' Evaluate formula on data
#'
#' This function evaluates formula on data and returns evaluated data.
#'
#' @param formula Formula to be evaluated
#' @param data \code{data.frame} for the formula
#' @param ... Other params to be passed to \code{model.frame}
#' @return Evaluated formula result
eval_formula <- function(formula, data, ...) {
  if (formula[[2L]] == ".") {
    rhs <- as.list(attr(stats::terms(formula[-2L]), "variables")[-1])
    lhs <- as.call(c(quote(cbind), setdiff(lapply(names(data),
                                                  as.name), rhs)))
    formula[[2L]] <- lhs
  }
  mf <- stats::model.frame(formula, data, ...)
  lhs <- if (is.matrix(mf[[1L]])) as.data.frame(mf[[1L]]) else mf[1L]
  return(list(x = lhs, by = mf[-1]))
}
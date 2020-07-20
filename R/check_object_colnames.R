#' Check object colnames
#'
#' If column names of the object is not set, column names of the \code{object}
#' are set by \code{object} names. If the \code{object} does not have dimension,
#' it's dimension set as 1 column.
#'
#' @keywords internal
check_object_colnames <- function(object) {
  lab <- "V"
  if (NCOL(object) == 1L) {
    if (is.null(dim(object))) dim(object) <- c(NROW(object), 1L)
    if (is.null(colnames(object))) colnames(object) <- lab
  }
  if (is.null(colnames(object)))
    colnames(object) <- paste0(lab, seq_len(NCOL(object)))
  object
}

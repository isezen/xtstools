% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reduce.R
\name{reduce}
\alias{reduce}
\alias{reduce.zoo}
\alias{reduce.data.frame}
\alias{reduce.formula}
\title{Reduce xts/zoo object}
\usage{
reduce(x, ..., which.i = FALSE)

\method{reduce}{zoo}(x, merge = TRUE, which.i = FALSE, on, ...)

\method{reduce}{data.frame}(x, merge = TRUE, which.i = FALSE, on, ...)

\method{reduce}{formula}(formula, data, which.i = FALSE, on, ...)
}
\arguments{
\item{x}{An \code{xts}/\code{zoo}/\code{data.frame} object.}

\item{...}{Unused}

\item{which.i}{If \code{TRUE}, returns indices.}

\item{merge}{if \code{merge = TRUE} and \code{x} has multiple \code{numeric}
columns, the result indices are merged.}

\item{on}{Reducing period. If missing, period will be set to second smallest
period in the series.}

\item{formula}{Formula to be evaluated.}

\item{data}{\code{data.frame} for the formula.}
}
\value{
A list of indices/values of \code{xts}/\code{zoo}/\code{data.frame}
objects. If \code{merge = TRUE}, values in the list are merged.
}
\description{
This function reduces \code{xts}/\code{zoo}/\code{data.frame} object by the
second smallest period in the series if \code{length} of series is greater
than a specified threshold. \code{data.frame} must have at least one
\code{POSIXt} and one \code{numeric} column. If \code{data.frame} has a
\code{factor} column, evaluation is done by considering \code{factor} levels.
}
\section{Methods (by class)}{
\itemize{
\item \code{reduce(zoo)}: S3 method for \code{zoo} object

\item \code{reduce(data.frame)}: S3 method for \code{data.frame}

\item \code{reduce(formula)}: S3 method for \code{formula}

}}

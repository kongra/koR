# Copyright (c) Konrad Grzanek
# Created 2015-07-20

#' @import data.table
#' @useDynLib kongRa
#' @importFrom Rcpp sourceCpp
NULL

#' Reports the number of elements matching the predicate.
#'
#' @param  x The data object (vector, frame).
#' @param  pred The predicate.
#' @param  keepZeros For non-atomics. When TRUE the zero counts will be
#'                   kept.
#' @return A representation of counts.
#' @export
countMatching <- function(x, pred, ...) {
  UseMethod("countMatching")
}

#' @export
countMatching.default <- function(x, pred, ...) {
  sum(pred(x))
}

#' @export
countMatching.data.frame <- function(x, pred, keepZeros = FALSE) {
  result <- list()
  for (n in colnames(x)) {
    v <- sum(pred(x[[n]]))
    if (keepZeros || v != 0) result[[n]] <- v
  }
  result
}

#' Determine and return duplicated rows.
#'
#' \code{duplicatedBy} returns rows having same values in the specified by col.
#' @export
duplicatedBy <- function(x, by, ...) {
  UseMethod("duplicatedBy")
}

#' @export
duplicatedBy.data.table <- function(dt, by, ...) {
  i    <- which(duplicated(dt[[by]], ...))
  dups <- dt[i][[by]]
  dt[dt[[by]] %in% dups]
}

#' @export
duplicatedBy.data.frame <- function(df, by, ...) {
  i    <- which(duplicated(df[[by]], ...))
  dups <- df[i, by]
  df[df[[by]] %in% dups, ]
}

##' A wrapper around \code{skewnessRcpp} safe with respect to NAs.
##' @export
skewness <- function(x, na.rm = FALSE) {
  if (any(ina <- is.na(x))) {
    if (na.rm)
      x <- x[!ina]
    else return(NA)
  }
  skewnessRcpp(x)
}

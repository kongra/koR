# Copyright (c) Konrad Grzanek
# Created 2015-07-20

#' Reports the number of elements matching the predicate.
#'
#' @param  .data The data object (vector, frame).
#' @param  pred The predicate.
#' @param  keepZeros For non-atomics. When TRUE the zero counts will be
#'                   kept.
#' @return A representation of counts.
#' @export
countMatching <- function(.data, pred, ...) {
  UseMethod("countMatching")
}

#' @export
countMatching.default <- function(.data, pred, ...) {
  sum(pred(.data))
}

#' @export
countMatching.data.frame <- function(.data, pred, keepZeros = FALSE) {
  result <- list()
  for (n in colnames(.data)) {
    v <- sum(pred(.data[[n]]))
    if (keepZeros || v != 0) result[[n]] <- v
  }
  result
}

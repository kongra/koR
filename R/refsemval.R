# Copyright (c) Konrad Grzanek
# Created 2018-07-10
#

#' @export
makeR <- function(x) {
  if (is.null(x)) stop("NULL arg is not allowed,", chR::errMessage(x))
  list("d3R3f" = x)
}

#' @export
makeV <- function(x) {
  if (is.null(x)) stop("NULL arg is not allowed,", chR::errMessage(x))
  list("d3V3f" = x)
}

#' @export
isR <- function(x) is.list(x) && !is.null(.subset2(x, "d3R3f"))

#' @export
isV <- function(x) is.list(x) && !is.null(.subset2(x, "d3V3f"))

copyDeref <- function(x) UseMethod("copyDeref")
copyDeref.data.table <- data.table::copy

#' @export
asR <- function(x, copy = TRUE) {
  if (isR(x))
    x
  else {
    x <- (if (is.list(x)) .subset2(x, "d3V3f")) %or% x
    makeR(if (copy) copyDeref(x) else x)
  }
}

#' @export
asV <- function(x, copy = TRUE) {
  if (isV(x))
    x
  else {
    x <- (if (is.list(x)) .subset2(x, "d3R3f")) %or% x
    makeV(if (copy) copyDeref(x) else x)
  }
}

#' @export
chR <- function(check, x) {
  deref <- if (is.list(x)) .subset2(x, "d3R3f")
  if (is.null(deref)) stop(chR::errMessage(x))
  check(deref)
}

#' @export
chV <- function(check, x) {
  deref <- if (is.list(x)) .subset2(x, "d3V3f")
  if (is.null(deref)) stop(chR::errMessage(x))
  check(deref)
}

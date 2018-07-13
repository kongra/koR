# Copyright (c) Konrad Grzanek
# Created 2018-07-10
#

#' @return chR(...)
#' @export
makeR <- function(x) {
  if (is.null(x)) stop("NULL arg is not allowed,", chR::errMessage(x))
  list("d3R3f" = x)
}

#' @return chV(...)
#' @export
makeV <- function(x) {
  if (is.null(x)) stop("NULL arg is not allowed,", chR::errMessage(x))
  list("d3V3f" = x)
}

#' @return chBool
#' @export
isR <- function(x) is.list(x) && !is.null(.subset2(x, "d3R3f"))

#' @return chBool
#' @export
isV <- function(x) is.list(x) && !is.null(.subset2(x, "d3V3f"))

copyDeref <- function(x) UseMethod("copyDeref")
copyDeref.data.table <- data.table::copy

#' @return chR(...)
#' @export
asR <- function(x, copy = TRUE) {
  if (isR(x))
    x
  else {
    x <- (if (is.list(x)) .subset2(x, "d3V3f")) %or% x
    makeR(if (copy) copyDeref(x) else x)
  }
}

#' @return chV(...)
#' @export
asV <- function(x, copy = TRUE) {
  if (isV(x))
    x
  else {
    x <- (if (is.list(x)) .subset2(x, "d3R3f")) %or% x
    makeV(if (copy) copyDeref(x) else x)
  }
}

#' @return x the arg
#' @export
chR <- function(check, x) {
  deref <- if (is.list(x)) .subset2(x, "d3R3f")
  if (is.null(deref)) stop(chR::errMessage(x))
  check(deref)
}

#' @return x the arg
#' @export
chV <- function(check, x) {
  deref <- if (is.list(x)) .subset2(x, "d3V3f")
  if (is.null(deref)) stop(chR::errMessage(x))
  check(deref)
}

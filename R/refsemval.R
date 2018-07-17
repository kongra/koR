# Copyright (c) Konrad Grzanek
# Created 2018-07-10
#

#' @return chR(...)
#' @export
makeR <- function(x) {
  if (is.null(x)) stop("NULL arg is not allowed,", chR::errMessage(x))
  list("d3R3f" = x)
}

#' @return chBool
#' @export
isR <- function(x) is.list(x) && !is.null(.subset2(x, "d3R3f"))

#' @export
unsafeR <- function(x) {
  d <- .subset2(x, "d3R3f")
  if (is.null(d)) stop("Not unsafeR(able),", chR::errMessage(x))
  d
}

#' @return chV(...)
#' @export
makeV <- function(x) {
  if (is.null(x)) stop("NULL arg is not allowed,", chR::errMessage(x))
  list("d3V3f" = x)
}

#' @return chBool
#' @export
isV <- function(x) is.list(x) && !is.null(.subset2(x, "d3V3f"))

#' @export
unsafeV <- function(x) {
  d <- .subset2(x, "d3V3f")
  if (is.null(d)) stop("Not unsafeV(able),", chR::errMessage(x))
  d
}

#' @export
safeV <- function(x) {
  d <- .subset2(x, "d3V3f")
  if (is.null(d)) stop("Not safeV(able),", chR::errMessage(x))
  copyV(d)
}

#' @export
copyV <- function(x) UseMethod("copyV")
copyV.data.table <- data.table::copy

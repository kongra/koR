# Copyright (c) Konrad Grzanek
# Created 2018-07-10
#

# DEFINE CLASSES WITH REFERENCE SEMANTICS HERE
#
REF_CLASSES <- c(
  "data.table"
)

copyDeref <- function(x) UseMethod("copyDeref")
copyDeref.data.table <- data.table::copy

# INSTRUMENTATION
#
for (rc in REF_CLASSES) setOldClass(rc)
setClassUnion("koR::RefClass", members = REF_CLASSES)

R <- setClass("koR::R", slots = list(deref = "koR::RefClass"))
V <- setClass("koR::V", slots = list(deref = "koR::RefClass"))

# API
#
#' @export
isR <- function(x) inherits(x, "koR::R")

#' @export
isV <- function(x) inherits(x, "koR::V")

#' @export
asR <- function(x, copy = TRUE) {
  if (isR(x))
    x
  else {
    x <- if (isV(x)) x@deref else x
    R(deref = if (copy) copyDeref(x) else x)
  }
}

#' @export
asV <- function(x, copy = TRUE) {
  if (isV(x))
    x
  else {
    x <- if (isR(x)) x@deref else x
    V(deref = if (copy) copyDeref(x) else x)
  }
}

#' @export
chR <- function(check, x) {
  if (!isR(x)) stop(chR::errMessage(x))
  check(x@deref)
  x
}

#' @export
chV <- function(check, x) {
  if (!isV(x)) stop(chR::errMessage(x))
  check(x@deref)
  x
}

# TESTS
#
# library(data.table)
# library(chR)
# library(koR)
# library(purrr)
# library(microbenchmark)
#
# dt1 <- data.table(x = 1L, y = 2L)
# r1  <- new("koR::R", deref = dt1)
# v1  <- asV(r1)
#
# microbenchmark(
#   asV(r1),
#   asV(asV(r1))
# )

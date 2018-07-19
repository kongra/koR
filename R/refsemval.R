# Copyright (c) Konrad Grzanek
# Created 2018-07-10
#

## VALUE SEMANTICS FOR OBJECTS WITH REFERENCES (data.table, env, etc.)

## INSTRUMENTATION
library(data.table)

REF_CLASSES <- c(
  "data.table"
)

safeCopy <- function(x) UseMethod("safeCopy")
safeCopy.data.table <- data.table::copy

for (rc in REF_CLASSES) setOldClass(rc)
setClassUnion("koR.RefClass", members = REF_CLASSES)

V <- setClass("koR.V", slots = list(unsafeV = "koR.RefClass"))

## API

#' @export
makeV <- function(x) V(unsafeV = x)

#' @export
isV <- function(x) inherits(x, "koR.V")

#' @export
chV <- function(check, x) {
  check(x@unsafeV)
  x
}

#' @export
safeV <- function(x) safeCopy(x@unsafeV)

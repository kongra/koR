# Copyright (c) Konrad Grzanek
# Created 2015-07-08

#' @import data.table
NULL

#' Alias for \code{pryr::object_size}
#' @export
osize  <- pryr::object_size

#' Alias for \code{pryr::mem_used}
#' @export
memuse <- pryr::mem_used

#' Alias for \code{microbenchmark::microbenchmark}
#' @export
timeit <- microbenchmark::microbenchmark

#' Execute gc multiple times.
#'
#' \code{rgc} Calls gc() n times, 1 when n is negative.
#'
#' @param n Number of calls.
#' @return The result of the last gc() call.
#' @export
rgc <- function(n = 10) {
  if (n > 1) {
    for (i in 1:(n - 1)) {
      gc()
    }
  }
  gc()
}

#' Condition handler generator.
#'
#' \code{printCondition} returns a condition handler that prints a
#' prefixed conditionMessage.
#'
#' @param prefix Prefix for the condition message.
#' @return A condition handler.
#' @export
printCondition <- function(prefix = "ERROR: ") {
  function(e) {
    print(paste(prefix, conditionMessage(e), sep = ""))
  }
}

#' Returns a data.table of n most frequent elements in vector x
#' together with their frequencies.
#' @export
mostFrequent <- function(x, n = 10) {
  dt <- data.table(x = x)
  head(dt[, .(freq = .N), by = x][order(-freq)], n)
}

#' Returns a data.table of mode values, together with their frequency.
#' @export
modes <- function(x) {
  dt1 <- data.table(x = x)
  dt2 <- dt1[, .(freq = .N), by = x]
  maxFreq = max(dt2$freq)
  dt2[freq == maxFreq, ]
}

#' Defaults for NULL values.
#' @export
`%or%` <- function(x, y) if (is.null(x)) y else x

#' Remove NULLs from a list.
#' @param x The list
#' @export
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

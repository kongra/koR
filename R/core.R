# Copyright (c) Konrad Grzanek
# Created 2015-07-08

#' @import data.table
#' @import chR
NULL

#' Alias for \code{pryr::object_size}
#' @export
osize <- pryr::object_size

#' Alias for \code{pryr::mem_used}
#' @export
memuse <- pryr::mem_used

#' Alias for \code{pryr::address}
#' @export
addr <- pryr::address

#' Alias for \code{microbenchmark::microbenchmark}
#' @export
qbench <- microbenchmark::microbenchmark

#' \code{cat}s (logs) time elapsed for expr to evaluate
#' @param expr expression to be evaluated
#' @param msg  optional message for easier identification of what was evaluated
#' @param off  optional flag for turning off when needed
#' @return     the value of evaluated expr
#' @export
catimela <- function(expr, msg = "", off = FALSE) {
  if (off)
    expr
  else {
    start <- Sys.time()
    value <- expr # expr is lazy, the evaluation takes place here
    end   <- Sys.time()
    cat({ if (msg != "") paste0(msg, " elapsed") else "elapsed" },
        as.double(end - start) * 1e3, "msecs\n")
    value
  }
}

#' Works like \code{str(object, ...)} but prints to String rather than to out
#' @export
strs <- function(object, ...) chString({
  stringr::str_trim(capture.output(str(object, ...)))
})

#' Execute gc multiple times.
#'
#' \code{rgc} Calls gc() n times, 1 when n is negative.
#'
#' @param n Number of calls.
#' @return The result of the last gc() call.
#' @export
rgc <- function(n = 10L) chUnit({
  chPosInt(n)
  if (n > 1) {
    for (i in 1:(n - 1)) {
      gc()
    }
  }
  gc()
  NULL
})

#' Condition handler generator.
#'
#' \code{printCondition} returns a condition handler that prints a
#' prefixed conditionMessage.
#'
#' @param prefix Prefix for the condition message.
#' @return A condition handler.
#' @export
printCondition <- function(prefix = "ERROR: ") chFun({
  chString(prefix)
  function(e) {
    print(paste(prefix, conditionMessage(e), sep = ""))
  }
})

#' Debug wrapper generator.
#'
#' Returns a diagnostic wrapper around f. Thanks H. Wickham.
#'
#' @param f Function to wrap
#' @param prefix A prefix for the diagnostic message
#' @export
chatty <- function(f, prefix = "Processing ") chFun({
  chFun(f)
  chString(prefix)
  function(x, ...) {
    res <- f(x, ...)
    cat(prefix, x, "\n", sep = "")
    res
  }
})

#' Returns a data.table of n most frequent elements in vector x
#' together with their frequencies.
#' @export
mostFrequent <- function(x, n = 10) chDT({
  chVector(x)
  dt <- data.table(x = x)
  head(dt[, .(freq = .N), by = x][order(-freq)], n)
})

#' Returns a data.table of mode values, together with their frequency.
#' @export
modes <- function(x) chDT({
  chVector(x)
  dt      <- data.table(x = x)[, .(freq = .N), by = x]
  maxFreq <- max(dt$freq)
  dt[freq == maxFreq]
})

#' Defaults for NULL values.
#' @export
`%or%` <- function(x, y) if (is.null(x)) y else x

#' Gets user installed packages.
#' @return a data table containing all the relevant information
#' @seealso installed.packages()
#' @export
userInstalledPackages <- function() chDT({
  as.data.table(installed.packages())[is.na(Priority)]
})

#' Returns a a decorator of f that prints . every n.
#' @export
dotEvery <- function(f, n) {
  chFun(f)
  chPosInt(n)
  i <- 1L
  function(...) {
    if (i %% n == 0) cat(".")
    i <<- i + 1
    f(...)
  }
}

#' Returns a decorator of f that sleeps time (in seconds) before executing f.
#' @export
sysSleeping <- function(f, time) chFun({
  chFun(f)
  chPosInt(time)
  function(...) {
    Sys.sleep(time)
    f(...)
  }
})

#' Puts all key/value mapping pairs into m
#' @param m associative container, list or env
#' @param kvs list of key/value pairs (must be even in size)
#' @return updated m
#' @export
assocKvs <- function(m, kvs) chRecursive({
  chRecursive(m)
  chList   (kvs)
  n   <- chEvenInt(length(kvs))
  if (n > 0L) {
    for (i in (1:(n/2))) {
      k      <- kvs[[2 * i - 1]]
      v      <- kvs[[2 * i]]
      m[[k]] <- v
    }
  }
  m
})

#' Creates a list using the enumerated collection of key/value mapping pairs
#' @param ... collection of key/value pairs (must be even in size)
#' @return the resulting list
#' @export
createListMap <- function(...) chList({
  assocKvs(list(), list(...))
})

#' Creates an env using the enumerated collection of key/value mapping pairs
#' @param ... collection of key/value pairs (must be even in size)
#' @return the resulting env
#' @export
createEnvMap <- function(...) chEnv({
  assocKvs(new.env(hash = TRUE), list(...))
})

#' Splits xs into partitions of length n, thanks to
#' https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
#' @param xs an atomic vector to partition
#' @param n  the partition size
#' @return a list containing the partitions
#' @export
partition <- function(xs, n) chList({
  chAtomic(xs)
  chPosInt(n)
  split(xs, ceiling(seq_along(xs) / n))
})

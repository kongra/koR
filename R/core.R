# Copyright (c) Konrad Grzanek
# Created 2015-07-08
#

# PUT ALL IMPORTS HERE
#' @import data.table
#' @import assertthat
#' @import stringr
#' @import ggplot2
#' @import grid
#' @import chR
#' @import RColorBrewer
#' @useDynLib koR
#' @importFrom Rcpp sourceCpp
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

#' \code{cat}s (logs) time elapsed for expr to evaluate.
#' @param expr expression to be evaluated
#' @param msg  optional message for easier identification of what was evaluated
#' @param off  optional flag for turning off when needed
#' @return     the value of evaluated expr
#' @export
catimela <- function(expr, msg = "", off = FALSE) {
  if (off)
    expr
  else {
    start <- as.double(Sys.time())
    force(expr)
    value <- expr
    end   <- as.double(Sys.time())
    pfx   <- if (msg != "") paste0(msg, " elapsed") else "elapsed"
    cat(pfx, (end - start) * 1e3, "msecs\n")
    value
  }
}

#' Works like \code{str(object, ...)} but prints to String rather than to out.
#' @export
strs <- function(object, ...) chString({
  stringr::str_trim(capture.output(str(object, ...)))
})

#' Execute gc multiple times.
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
#' \code{printCondition} returns a condition handler that prints a
#' prefixed conditionMessage.
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
#' Returns a diagnostic wrapper around f. Thanks H. Wickham.
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

#' Defaults for NULL value.
#' @export
`%or%` <- function(x, y) if (is.null(x)) y else x

#' Defaults for a single NA value.
#' @export
`%NAor%` <- function(x, y) if (is.na(x)) y else x

#' Works like == but for NA == NA returns TRUE and not NA (like ==).
#' @export
`%==NA%` <- function(x, y) ifelse(is.na(x) & is.na(y), TRUE, x == y)

#' Returns TRUE iff |x - y| <= e
#' @export
epsiEqual <- function(x, y, e = 0.00001) abs(x - y) <= e

#' Returns TRUE iff |x - y| <= 0.1.
#' @export
`%==e1%`  <- function(x, y) epsiEqual(x, y, e =   0.1)

#' Returns TRUE iff |x - y| <= 0.01.
#' @export
`%==e2%`  <- function(x, y) epsiEqual(x, y, e =  0.01)

#' Returns TRUE iff |x - y| <= 0.001.
#' @export
`%==e3%`  <- function(x, y) epsiEqual(x, y, e = 0.001)

#' Returns TRUE iff |x - y| <= 1e-4.
#' @export
`%==e4%`  <- function(x, y) epsiEqual(x, y, e =  1e-4)

#' Returns TRUE iff |x - y| <= 1e-5.
#' @export
`%==e5%`  <- function(x, y) epsiEqual(x, y, e =  1e-5)

#' Returns TRUE iff |x - y| <= 1e-6.
#' @export
`%==e6%`  <- function(x, y) epsiEqual(x, y, e =  1e-6)

#' Returns TRUE iff |x - y| <= 1e-7.
#' @export
`%==e7%`  <- function(x, y) epsiEqual(x, y, e =  1e-7)

#' Returns TRUE iff |x - y| <= 1e-8.
#' @export
`%==e8%`  <- function(x, y) epsiEqual(x, y, e =  1e-8)

#' Returns TRUE iff |x - y| <= 1e-9.
#' @export
`%==e9%`  <- function(x, y) epsiEqual(x, y, e =  1e-9)

#' Returns TRUE iff |x - y| <= 1e-12.
#' @export
`%==e12%` <- function(x, y) epsiEqual(x, y, e = 1e-12)

#' Returns TRUE iff |x - y| <= 1e-16.
#' @export
`%==e16%` <- function(x, y) epsiEqual(x, y, e = 1e-16)

#' @export
doublesProximity <- function(x, y, ...) chDouble({
  chDoubles(x)
  chDoubles(y)
  proxy::simil(list(x, y), ...)[1]
})

#' @export
nonFinite2NAs <- function(xs) { # chDoubles
  chNumerics(xs)
  if (length(xs) == 0L)
    double()
  else
    ifelse(is.finite(xs), xs, NA_real_)
}

#' @export
percDiffs <- function(x, y) chDoubles({
  ifelse(is.na(x) | is.na(y),
    NA_real_, # x = NA | y = NA => NA
  # else
    {
      ifelse(y == 0.0,
        ifelse(x == 0.0,
          0.0,       # x =  0 & y = 0 => 0
        # else
          NA_real_), # x <> 0 & y = 0 => NA
      # else
        (x - y) / y * 100.0)
    })
})

#' @export
safeBool <- function(b) chBool({
  chAtomic(b)
  if (length(b) == 0 || # length(NULL) is 0
      is.na (b))
    FALSE
  else
    as.logical(b)
})

#' @export
safeMin <- function(xs, na.rm = TRUE) {
  if (na.rm) xs <- xs[!is.na(xs)]
  if (length(xs) == 0L) Inf else base::min(xs)
}

#' @export
safeMax <- function(xs, na.rm = TRUE) {
  if (na.rm) xs <- xs[!is.na(xs)]
  if (length(xs) == 0L) -Inf else base::max(xs)
}

#' @export
minNW <- function(...) withCallingHandlers(
  suppressWarnings(base::min(...)),
  warning = function(w) {})

#' @export
maxNW <- function(...) withCallingHandlers(
  suppressWarnings(base::min(...)),
  warning = function(w) {})

#' @export
atomicsNthFirst <- function(nth = 1L) chFun({
  chPosInt(nth)
  function(xs) chScalar({
    chAtomic(xs)
    xs[nth]
  })
})

#' @export
atomicsNthLast <- function(nth = 1L) chFun({
  chPosInt(nth)
  function(xs) chScalar({
    chAtomic(xs)
    rev(xs)[nth]
  })
})

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

#' Puts all key/value mapping pairs into m.
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

#' Creates a list using the enumerated collection of key/value mapping pairs.
#' @param ... collection of key/value pairs (must be even in size)
#' @return the resulting list
#' @export
createListMap <- function(...) chList({
  assocKvs(list(), list(...))
})

#' Returns an empty hash map implemented as an environment.
#' @export
emptyHashMap <- function() chEnv({
  new.env(hash = TRUE, parent = emptyenv())
})

#' Creates an env using the enumerated collection of key/value mapping pairs.
#' @param ... collection of key/value pairs (must be even in size)
#' @return the resulting env
#' @export
hashMap <- function(...) chEnv({
  assocKvs(emptyHashMap(), list(...))
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

#' Sets class(x) to cls.
#' @param x an object to set the class c to
#' @param cls a class
#' @return the x argument with class(x) set to cls
#' @export
ofClass <- function(x, cls) {
  class(x) <- cls
  x
}

#' Adds cls to class(x).
#' @param x an object to add the class cls to
#' @param cls a class
#' @return the x argument
#' @export
addClass <- function(x, cls) {
  if (inherits(x, cls))
    x
  else {
    class(x) <- c(class(x), cls)
    x
  }
}

#' Removes (discards) elements in list l for which p is TRUE.
#' @param l a list
#' @param p a predicate
#' @return  a resulting list
#' @export
listDiscard <- function(l, p) l[!sapply(l, p)]

#' Keeps elements in list l for which p is TRUE.
#' @param l a list
#' @param p a predicate
#' @return  a resulting list
listKeep <- function(l, p) l[sapply(l, p)]

#' Works like \code{do.call} but optionaly removes all NULL args.
#' @export
doCall <- function(what, args, quote = FALSE, envir = parent.frame(), null.rm = TRUE) {
  if (null.rm) args <- listDiscard(args, is.null)
  do.call(what = what, args = args, quote = quote, envir = envir)
}

#' Short-circuit evaluator for Boolean values
#' @param b a boolean vector of some length n
#' @param x a vector of lenght presumably equal to n
#' @param pred a predicate to evaluate on these elements of x for which b is TRUE
#' @return a boolean vector of length n
#' @export
boolsAnd <- function(b, x, pred) {
  if (sum(b) == 0L) b else boolsAndInterveawe(b, pred(x[b]))
}

#' Peformance optimized base::setdiff
#' @param x an input vector
#' @param y vector of elements to filter out from x
#' @return x without elements of y
#' @export
disj <- function(x, y) {
  if (length(y) == 1L)
    x[x != y]
  else
    x[!(x %in% y)]
}

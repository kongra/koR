# Copyright (c) Konrad Grzanek
# Created 2015-07-08

#' Alias for \code{pryr::object_size}
osize  <- pryr::object_size

#' Alias for \code{pryr::mem_used}
memuse <- pryr::mem_used

#' Alias for \code{microbenchmark::microbenchmark}
timeit <- microbenchmark::microbenchmark

#' Execute gc multiple times.
#'
#' \code{rgc} Calls gc() n times, 1 when n is negative.
#'
#' @param n Number of calls.
#' @return The result of the last gc() call.
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
printCondition <- function(prefix = "ERROR: ") {
  function(e) {
    print(paste(prefix, conditionMessage(e), sep = ""))
  }
}

#' Returns a data-frame of n most frequent elements in vector x
#' together with their frequencies.
mostFrequent <- function(x, n = 10) {
  data_frame(x) %>% group_by(x) %>% summarise(freq = n()) %>%
    arrange(desc(freq)) %>% head(n)
}

#' Returns a data-frame of mode values, together with their frequency.
modes <- function(x) {
  df <- data_frame(x) %>% group_by(x) %>% summarise(freq = n())
  df %>% filter(freq == max(df$freq))
}

#' Defaults for NULL values.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Remove NULLs from a list.
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# Copyright (c) Konrad Grzanek
# Created 2018-01-12 12_04_55 CEST
#

#' @import chR
#' @import stringr
NULL

#' Inserts what before positive index i in string s. Vectorized over all args.
#' @param s a string to transform
#' @param i a positive index
#' @param what to insert
#' @return a transformed string
#' @export
strInsertBeforeIndex <- function(s, i, what) chStrings({
  chStrings(s)
  chPosInts(i)
  chStrings(what)
  paste0(str_sub(s, 0L, i - 1L), what, str_sub(s, i))
})

#' Inserts what after a non-negative index i in string s. Vectorized over all args.
#' @param s a string to transform
#' @param i a non-negative index
#' @param what to insert
#' @return a transformed string
#' @export
strInsertAfterIndex <- function(s, i, what) chStrings({
  chStrings(s)
  chNatInts(i)
  chStrings(what)
  paste0(str_sub(s, 0L, i), what, str_sub(s, i + 1L))
})

#' Wraps every occurence of the pattern with a prefix and a suffix. Non-vectorized,
#' the arguments are expected to be scalars (atoms).
#' @param s a string to transform
#' @param pattern describing the pieces of s to wrap
#' @param prefix : chString
#' @param suffix : chString
#' @param ignoreCase : chBool
#' @return a transformed string
#' @export
strWrapAll <- function(s, pattern, prefix, suffix, ignoreCase = TRUE) {
  chString(s)
  chString(pattern)
  chString(prefix)
  chString(suffix)
  chBool  (ignoreCase)

  occurences <- str_locate_all(s, fixed(pattern, ignore_case = ignoreCase))[[1]]
  N <- nrow(occurences)
  if (N == 0L)
    s
  else {
    off  <- 0L
    INC1 <- str_length(prefix)
    INC2 <- str_length(suffix)
    for (i in 1:N) {
      start <- occurences[i, 1]
      end   <- occurences[i, 2]
      s     <- s %>% strInsertBeforeIndex(start + off, prefix)
      off   <- off + INC1
      s     <- s %>% strInsertAfterIndex(end + off, suffix)
      off   <- off + INC2
    }
    s
  }
}

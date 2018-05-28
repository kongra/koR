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

TRIMMED_CLASS    <- "koR::Trimmed"
#' @export
chTrimmedStrings <- chInstance (TRIMMED_CLASS)
#' @export
chTrimmedString  <- chInstance1(TRIMMED_CLASS)

#' Trims given strings (both ends)
#' @param s a vector of strings to trim
#' @return a vector of trimmed strings
#' @export
trimStrings <- function(s) {
  if (inherits(s, TRIMMED_CLASS))
    s
  else {
    chStrings(s)
    ofClass(if (length(s) < 150L) trimws(s) else str_trim(s),
            TRIMMED_CLASS)
  }
}

#' Explicitly marks given strings as trimmed. User takes responsibility.
#' @param s a string to mark as trimmed
#' @return s
#' @export
asTrimmed <- function(s) {
  chStrings(s)
  addClass(s, TRIMMED_CLASS)
}

NON_BLANKS_CLASS <- "koR::NonBlanks"
#' @export
chNonBlanks <- chInstance (NON_BLANKS_CLASS)
#' @export
chNonBlank  <- chInstance1(NON_BLANKS_CLASS)

#' Explicitly marks given strings as trimmed. User takes responsibility.
#' @param s a string to mark as trimmed
#' @return s
#' @export
asNonBlanks <- function(s) {
  chStrings(s)
  addClass(s, NON_BLANKS_CLASS)
}

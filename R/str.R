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
      s     <- strInsertBeforeIndex(s, start + off, prefix)
      off   <- off + INC1
      s     <- strInsertAfterIndex(s, end + off, suffix)
      off   <- off + INC2
    }
    s
  }
}

TRIMMED_CLASS    <- "koR::Trimmed"
#' @export
chTrimmeds <- chInstance (TRIMMED_CLASS)
#' @export
chTrimmed  <- chInstance1(TRIMMED_CLASS)

#' Trims given strings (both ends)
#' @param s a vector of strings to trim
#' @return a vector of trimmed strings
#' @export
trimmeds <- function(s) {
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
asTrimmeds <- function(s) {
  chStrings(s)
  ofClass(s, TRIMMED_CLASS)
}

NON_BLANK_CLASS <- "koR::NonBlank"
#' @export
chNonBlanks <- chInstance (NON_BLANK_CLASS)
#' @export
chNonBlank  <- chInstance1(NON_BLANK_CLASS)

#' Explicitly marks given strings as trimmed. User takes responsibility.
#' @param s a string to mark as trimmed
#' @return s
#' @export
asNonBlanks <- function(s) {
  chStrings(s)
  ofClass(s, NON_BLANK_CLASS)
}

#' Asserts non-blankness and returns koR::Trimmed strings as NonBlank.
#' @param s koR::Trimmed strings
#' @return koR::NonBlank strings
#' @export
nonBlanks <- function(s) {
  if (inherits(s, NON_BLANK_CLASS))
    s
  else {
    chTrimmeds(s)
    i <- which(s == "")
    if (length(i) != 0L) stop("Blank string(s) at ", i)
    ofClass(s, NON_BLANK_CLASS)
  }
}

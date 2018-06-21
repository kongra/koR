# Copyright (c) Konrad Grzanek
# Created 2018-06-18
#

# FORMATTERS API
#
setClass("koR::Fmt", slots = list(
  ch          = "function",
  fromStrings = "function",
  toStrings   = "function",
  fromUxs     = "function",
  toUxs       = "function"
))

#' @export
fmt <- function(ch, fromStrings, toStrings, fromUxs, toUxs) {
  chFun(ch)
  chFun(fromStrings)
  chFun(toStrings)
  chFun(fromUxs)
  chFun(toUxs)

  new("koR::Fmt",
      ch          = ch,
      fromStrings = fromStrings,
      toStrings   = toStrings,
      fromUxs     = fromUxs,
      toUxs       = toUxs)
}

#' @export
chFmt <- chInstance("koR::Fmt")

#' @export
fmtFromStrings <- function(fmt, s, ...) {
  chStrings(s)
  result <- fmt@fromStrings(s, ...)
  fmt@ch(result)
}

#' @export
fmtToStrings <- function(fmt, x, ...) {
  fmt@ch(x)
  chStrings(fmt@toStrings(x))
}

#' @export
fmtFromUxs <- function(fmt, s, ...) {
  chStrings(s)
  result <- fmt@fromUxs(s, ...)
  fmt@ch(result)
}

#' @export
fmtToUxs <- function(fmt, x, ...) {
  fmt@ch(x)
  chStrings(fmt@toUxs(x))
}

# COMMON FORMATTERS
#
#' @export
FmtStrings <- fmt(ch          = chStrings,
                  fromStrings = base::identity,
                  toStrings   = base::identity,
                  fromUxs     = base::identity,
                  toUxs       = base::identity)

#' @export
FmtInts <- fmt(ch             = chInts,
               fromStrings    = as.integer,
               toStrings      = as.character,
               fromUxs        = as.integer,
               toUxs          = as.character)

#' @export
FmtNatInts <- fmt(ch          = chNatInts,
                  fromStrings = as.integer,
                  toStrings   = as.character,
                  fromUxs     = as.integer,
                  toUxs       = as.character)

#' @export
FmtPosInts <- fmt(ch          = chPosInts,
                  fromStrings = as.integer,
                  toStrings   = as.character,
                  fromUxs     = as.integer,
                  toUxs       = as.character)

#' @export
FmtDoubles <- fmt(ch          = chDoubles,
                  fromStrings = as.double,
                  toStrings   = as.character,
                  fromUxs     = as.double,
                  toUxs       = as.character)

#' @export
FmtBools <- fmt(ch            = chBools,
                fromStrings   = as.logical,
                toStrings     = as.character,
                fromUxs       = as.logical,
                toUxs         = as.character)

#' @export
FmtDates <- fmt(ch            = chDates,
                fromStrings   = as.Date,
                toStrings     = as.character,
                fromUxs       = as.Date,
                toUxs         = as.character)



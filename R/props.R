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
  toUxs       = "function",
  ident       = "logical"
))

#' @export
fmt <- function(ch, fromStrings, toStrings, fromUxs, toUxs, ident = FALSE) {
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
      toUxs       = toUxs,
      ident       = ident)
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
fmt2Strings <- function(fmt, x, ...) {
  fmt@ch(x)
  chStrings(fmt@toStrings(x, ...))
}

#' @export
fmtFromUxs <- function(fmt, s, ...) {
  chStrings(s)
  result <- fmt@fromUxs(s, ...)
  fmt@ch(result)
}

#' @export
fmt2Uxs <- function(fmt, x, ...) {
  fmt@ch(x)
  chStrings(fmt@toUxs(x, ...))
}

# COMMON FORMATTERS
#
#' @export
FmtStrings <-
  fmt(ch          = chStrings,
      fromStrings = base::identity,
      toStrings   = base::identity,
      fromUxs     = base::identity,
      toUxs       = base::identity)

#' @export
FmtInts <-
  fmt(ch          = chInts,
      fromStrings = as.integer,
      toStrings   = as.character,
      fromUxs     = as.integer,
      toUxs       = as.character)

#' @export
FmtNatInts <-
  fmt(ch          = chNatInts,
      fromStrings = as.integer,
      toStrings   = as.character,
      fromUxs     = as.integer,
      toUxs       = as.character)

#' @export
FmtPosInts <-
  fmt(ch          = chPosInts,
      fromStrings = as.integer,
      toStrings   = as.character,
      fromUxs     = as.integer,
      toUxs       = as.character)

#' @export
FmtDoubles <-
  fmt(ch          = chDoubles,
      fromStrings = as.double,
      toStrings   = as.character,
      fromUxs     = as.double,
      toUxs       = as.character)

#' @export
FmtBools <-
  fmt(ch          = chBools,
      fromStrings = as.logical,
      toStrings   = as.character,
      fromUxs     = as.logical,
      toUxs       = as.character)

#' @export
FmtDates <-
  fmt(ch          = chDates,
      fromStrings = as.Date,
      toStrings   = as.character,
      fromUxs     = as.Date,
      toUxs       = as.character)

# FORMATTING SUPPORT
#
#' @export
safe2Strings <- function(f) {
  function(xs, ...) {
    if (length(xs) == 0L)
      character(0L)
    else
      ifelse(is.na(xs),
        NA_character_,
      # else
        chStrings(f(xs, ...)))
  }
}

formatNumerics <- safe2Strings(function(xs, dp = 2L) { # chStrings
  chNumerics(xs)
  chNatInt  (dp)
  trimws(format(round(xs, dp), nsmall = dp, scientific = FALSE))
})

# OTHER FORMATTERS
#

#' @export
FmtUSDates <- {
  LOCALE      <- readr::locale(date_format = "%m/%d/%Y")
  fromStrings <- function(s) readr::parse_date(s, locale = LOCALE)
  toStrings   <- function(d) format(d, "%m/%d/%Y")

  fmt(ch          = chDates,
      fromStrings = fromStrings,
      toStrings   = toStrings,
      fromUxs     = fromStrings,
      toUxs       = toStrings)
}

#' @export
FmtUSD <-
  fmt(ch          = chDoubles,
      fromStrings = as.double,
      toStrings   = as.character,

      fromUxs = function(s) {
        s <- str_replace    (s, "\\$", "")
        s <- str_replace_all(s, ",",   "")
        suppressWarnings(as.double(s))
      },

      toUxs = safe2Strings(scales::dollar_format(
        prefix             = "",
        largest_with_cents = .Machine$integer.max)))

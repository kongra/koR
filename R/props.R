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
chFmt <- chInstance("koR::Fmt")

#' @export
fmt <- function(ch, fromStrings, toStrings, fromUxs, toUxs, ident = FALSE) { # chFmt
  chBool(ident)
  new("koR::Fmt",
      ch          = ch,
      fromStrings = fromStrings,
      toStrings   = toStrings,
      fromUxs     = fromUxs,
      toUxs       = toUxs,
      ident       = ident)
}

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
      toUxs       = base::identity,
      ident       = TRUE)

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

#' @export
formatNumerics <- {
  NUM_FORMAT <- safe2Strings(function(xs, dp)
    trimws(format(round(xs, dp), nsmall = dp, scientific = FALSE)))

  safe2Strings(function(xs, dp = 2L, inf2NAs = TRUE) {
    chNatInt(dp)
    chBool  (inf2NAs)
    NUM_FORMAT(xs = if (inf2NAs) nonFinite2NAs(xs) else xs, dp = dp)
  })
}

#' @export
formatUSD <- {
  USD_FORMAT <- safe2Strings(scales::dollar_format(
    prefix             = "",
    largest_with_cents = .Machine$integer.max))

  safe2Strings(function(d, inf2NAs = TRUE) { # chStrings
    chBool(inf2NAs)
    USD_FORMAT(if (inf2NAs) nonFinite2NAs(d) else d)
  })
}

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
  fmt(ch          = chNumerics,
      fromStrings = as.numeric,
      toStrings   = as.character,

      fromUxs = function(s) {
        s <- str_replace    (s, "\\$", "")
        s <- str_replace_all(s, ",",   "")
        suppressWarnings(as.double(s))
      },

      toUxs = formatUSD)

#' @export
FmtNumerics <-
  fmt(ch          = chNumerics,
      fromStrings = as.numeric,
      toStrings   = as.character,
      fromUxs     = as.numeric,
      toUxs       = formatNumerics)

#' @export
FmtFactor <- {
  DEFAULT <- fmt(ch          = chFactors,
                 fromStrings = as.factor,
                 toStrings   = as.character,
                 fromUxs     = as.factor,
                 toUxs       = as.character)

  function(levels = NULL) {
    if (is.null(levels))
      DEFAULT
    else {
      chStrings(levels)
      asFactor <- function(x) factor(x = x, levels = levels)
      fmt(ch          = chFactors,
          fromStrings = asFactor,
          toStrings   = as.character,
          fromUxs     = asFactor,
          toUxs       = as.character)
    }
  }
}

# PROPS/PROPSETS API
#
setClass("koR::Prop", slots = list(
  fmt       = "koR::Fmt",
  transient = "logical"
))

setClass("koR::Propset", slots = list(
  props = "character",
  index = "list"
))

#' @export
chPropset <- chInstance("koR::Propset")

#' @export
prop <- function(name, fmt, transient = FALSE) { # chPropset
  chString(name)
  index <- list()
  index[[name]] <- new("koR::Prop", fmt = fmt, transient = transient)
  new("koR::Propset", props = name, index = index)
}

#' @export
propset <- function(...) { # chPropset
  args <- list(...)

  props <- purrr::reduce(purrr::map(args, function(a) a@props), c)
  dups  <- props[duplicated(props)]
  if (length(dups) > 0L) stop(
    "Duplicates occured: ", paste(dups, collapse = ", "))

  new("koR::Propset",
      props = props,
      index = purr::reduce(purrr::map(args, function(a) a@index), c))
}

# Info_PSET <- propset(
#   "Employee Number"     %>% prop(FmtPosInts),
#   "Employee First Name" %>% prop(FmtStrings),
#   "Employee Last Name"  %>% prop(FmtStrings)
# )
#
# Gross_PSET <- propset(
#   "$ Gross Wage" %>% prop(FmtUSD),
#   "$ Regular"    %>% prop(FmtUSD)
# )
#
# Cens_PSET <- propset(
#   Info_PSET,
#   # Info_PSET,
#   # Info_PSET,
#   # Info_PSET,
#   Gross_PSET,
#   # Gross_PSET,
#   # Gross_PSET,
#   # Gross_PSET,
#   "$ Hours" %>% prop(FmtNumerics)
# )
#
# microbenchmark::microbenchmark(
#   propset(
#     Info_PSET,
#     # Info_PSET,
#     # Info_PSET,
#     # Info_PSET,
#     Gross_PSET,
#     # Gross_PSET,
#     # Gross_PSET,
#     # Gross_PSET,
#     "$ Hours" %>% prop(FmtNumerics)
#   )
# )

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
fmt <- function(ch, fromStrings, toStrings, fromUxs, toUxs, ident = FALSE) chFmt({
  chBool(ident)
  new("koR::Fmt",
      ch          = ch,
      fromStrings = fromStrings,
      toStrings   = toStrings,
      fromUxs     = fromUxs,
      toUxs       = toUxs,
      ident       = ident)
})

#' @export
fmtFromStrings <- function(fmt, s, ...) {
  chStrings(s)
  fmt@ch(fmt@fromStrings(s, ...))
}

#' @return :chStrings
#' @export
fmt2Strings <- function(fmt, x, ...) {
  fmt@ch(x)
  chStrings(fmt@toStrings(x, ...))
}

#' @export
fmtFromUxs <- function(fmt, s, ...) {
  chStrings(s)
  fmt@ch(fmt@fromUxs(s, ...))
}

#' @return :chStrings
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
  function(xs, ...) { # chStrings
    if (length(xs) == 0L)
      character(0L)
    else
      ifelse(is.na(xs),
        NA_character_,
      # else
        chStrings(f(xs, ...)))
  }
}

#' @return :chStrings
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

#' @return :chStrings
#' @export
formatUSD <- {
  USD_FORMAT <- safe2Strings(scales::dollar_format(
    prefix             = "",
    largest_with_cents = .Machine$integer.max))

  safe2Strings(function(d, inf2NAs = TRUE) {
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
prop <- function(name, fmt, transient = FALSE) chPropset({
  chString(name)
  chBool  (transient)

  index <- list()
  index[[name]] <- new("koR::Prop", fmt = fmt, transient = transient)
  new("koR::Propset", props = name, index = index)
})

#' @export
propset <- function(...) chPropset({
  args <- list(...)

  props <- purrr::reduce(purrr::map(args, function(a) a@props), c)
  dups  <- props[duplicated(props)]
  if (length(dups) > 0L) stop(
    "Duplicates occured: ", paste(dups, collapse = ", "))

  new("koR::Propset",
      props = props,
      index = purrr::reduce(purrr::map(args, function(a) a@index), c))
})

#' @export
propsetDisj <- function(pset, names) chPropset({
  chPropset(pset)
  chStrings(names)

  index <- pset@index
  for (p in names) index[[p]] <- NULL

  new("koR::Propset",
      props = disj(pset@props, names),
      index = index)
})

#' @export
propsetKeep <- function(pset, names) chPropset({
  chPropset(pset)
  chStrings(names)

  # names MAY NOT be a subset of pset@props
  props <- pset@props
  index <- pset@index
  for (p in props) if (!(p %in% names)) index[[p]] <- NULL

  new("koR::Propset",
      props = props[props %in% names],
      index = index)
})

#' @export
propsetTransients <- function(pset) chStrings({
  chPropset(pset)
  index <- pset@index
  purrr::keep(pset@props, function(p) index[[p]]@transient)
})

#' @export
propsetNonTransients <- function(pset) chStrings({
  chPropset(pset)
  index <- pset@index
  purrr::keep(pset@props, function(p) !(index[[p]]@transient))
})

#' @return :chFmt
#' @export
propFmt <- function(name, pset) pset@index[[name]]@fmt

#' @return :chBool
#' @export
propTransient <- function(name, pset) pset@index[[name]]@transient

# SOME DT (data.table) UTILS
#

#' @export
propsetDTproject <- function(dt, pset) chDT({
  chDT     (dt)
  chPropset(pset)
  props <- pset@props
  dt[, ..props]
})

#' @export
overDTpropset <- function(dt, pset, f, ...) chDT({
  chDT     (dt)
  chPropset(pset)

  colNames <- colnames(dt)
  for (p in pset@props)
    if (p %in% colNames)
      overDT(dt, p, f, ...)
  dt
})

propsetDTfmt <- function(dt, pset, f, ...) chDT({
  chDT     (dt)
  chPropset(pset)

  colNames <- colnames(dt)
  for (p in pset@props)
    if (p %in% colNames) { # Always forgiving (skipMissing)
      fmt <- propFmt(p, pset)
      if (!fmt@ident) # When identity fmt, no need to do anything
        koR::setDT(dt, p, f(fmt, dt[[p]], ...))
    }

  dt
})

#' @return :chDT
#' @export
propsetDT2Strings <- function(dt, pset, ...) propsetDTfmt(dt, pset, fmt2Strings, ...)

#' @return :chDT
#' @export
propsetDTFromStrings <- function(dt, pset, ...) propsetDTfmt(dt, pset, fmtFromStrings, ...)

#' @return :chDT
#' @export
propsetDT2Uxs <- function(dt, pset, ...) propsetDTfmt(dt, pset, fmt2Uxs, ...)

#' @return :chDT
#' @export
propsetDTFromUxs <- function(dt, pset, ...) propsetDTfmt(dt, pset, fmtFromUxs, ...)

#' @return :chDT
#' @export
assertDTpropset <- function(dt, pset) {
  chPropset(pset)
  assertDTcols(dt, pset@props)
}

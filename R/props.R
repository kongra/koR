# Copyright (c) Konrad Grzanek
# Created 2018-06-18
#

# FORMATTERS API
#
setClass("koR::Fmt", slots = list(
  ch          = "function",
  typePred    = "function",
  coercer     = "function",
  fromStrings = "function",
  toStrings   = "function",
  fromUxs     = "function",
  toUxs       = "function",
  ident       = "logical"
))

#' @export
chFmt <- chInstance("koR::Fmt")

#' @export
fmt <- function(ch, typePred, coercer,
                fromStrings, toStrings, fromUxs, toUxs,
                ident = FALSE) chFmt({
  chBool(ident)
  new("koR::Fmt",
      ch          = ch,
      typePred    = typePred,
      coercer     = coercer,
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

#' @export
fmtCoerce <- function(fmt, x, fmtFrom = fmtFromStrings, ...) {
  if (fmt@typePred(x))
    x
  else if (is.character(x))
    fmtFrom(fmt, x, ...)
  else
    fmt@ch(fmt@coercer(x))
}

# COMMON FORMATTERS
#

#' @export
FmtStrings <-
  fmt(ch          = chStrings,
      typePred    = is.character,
      coercer     = as.character,
      fromStrings = base::identity,
      toStrings   = base::identity,
      fromUxs     = base::identity,
      toUxs       = base::identity,
      ident       = TRUE)

#' @export
FmtInts <-
  fmt(ch          = chInts,
      typePred    = is.integer,
      coercer     = as.integer,
      fromStrings = as.integer,
      toStrings   = as.character,
      fromUxs     = as.integer,
      toUxs       = as.character)

#' @export
FmtNatInts <-
  fmt(ch          = chNatInts,
      typePred    = function(x) is.integer(x) && areNatInts(x),
      coercer     = as.integer,
      fromStrings = as.integer,
      toStrings   = as.character,
      fromUxs     = as.integer,
      toUxs       = as.character)

#' @export
FmtPosInts <-
  fmt(ch          = chPosInts,
      typePred    = function(x) is.integer(x) && arePosInts(x),
      coercer     = as.integer,
      fromStrings = as.integer,
      toStrings   = as.character,
      fromUxs     = as.integer,
      toUxs       = as.character)

#' @export
FmtDoubles <-
  fmt(ch          = chDoubles,
      typePred    = is.double,
      coercer     = as.double,
      fromStrings = as.double,
      toStrings   = as.character,
      fromUxs     = as.double,
      toUxs       = as.character)

#' @export
FmtBools <-
  fmt(ch          = chBools,
      typePred    = is.logical,
      coercer     = as.logical,
      fromStrings = as.logical,
      toStrings   = as.character,
      fromUxs     = as.logical,
      toUxs       = as.character)

#' @export
FmtDates <-
  fmt(ch          = chDates,
      typePred    = function(x) inherits(x, "Date"),
      coercer     = as.Date,
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
      typePred    = function(x) inherits(x, "Date"),
      coercer     = as.Date,
      fromStrings = fromStrings,
      toStrings   = toStrings,
      fromUxs     = fromStrings,
      toUxs       = toStrings)
}

#' @export
FmtUSD <-
  fmt(ch          = chDoubles,
      typePred    = is.double,
      coercer     = as.double,
      fromStrings = as.double,
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
      typePred    = is.numeric,
      coercer     = as.numeric,
      fromStrings = as.numeric,
      toStrings   = as.character,
      fromUxs     = as.numeric,
      toUxs       = formatNumerics)

#' @export
FmtFactor <- {
  DEFAULT <- fmt(ch          = chFactors,
                 typePred    = is.factor,
                 coercer     = as.factor,
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
          typePred    = function(x) is.factor(x) && all(levels == base::levels(x)),
          coercer     = asFactor,
          fromStrings = asFactor,
          toStrings   = as.character,
          fromUxs     = asFactor,
          toUxs       = as.character)
    }
  }
}

# PROPS/PROPSETS API
#
setClass("koR::PropInfo", slots = list(
  fmt       = "koR::Fmt",
  transient = "logical"
))

setClass("koR::Propset", slots = list(
  props = "character",
  infos = "list"
))

#' @export
chPropset <- chInstance("koR::Propset")

#' @export
prop <- function(name, fmt, transient = FALSE) chPropset({
  chString(name)
  chBool  (transient)

  infos <- list()
  infos[[name]] <- new("koR::PropInfo", fmt = fmt, transient = transient)
  new("koR::Propset", props = name, infos = infos)
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
      infos = purrr::reduce(purrr::map(args, function(a) a@infos), c))
})

#' @export
propsetDisj <- function(pset, props) chPropset({
  chPropset(pset)
  chStrings(props)

  infos <- pset@infos
  for (p in props) infos[[p]] <- NULL

  new("koR::Propset",
      props = disj(pset@props, props),
      infos = infos)
})

#' @export
propsetKeep <- function(pset, props) chPropset({
  chPropset(pset)
  chStrings(props)

  # props MAY NOT be a subset of pset@props
  props <- props[props %in% pset@props]
  infos <- list()
  for (p in props) infos[[p]] <- pset@infos[[p]]
  new("koR::Propset", props = props, infos = infos)
})

#' @export
propsetTransients <- function(pset) chStrings({
  chPropset(pset)
  infos <- pset@infos
  purrr::keep(pset@props, function(p) infos[[p]]@transient)
})

#' @export
propsetNonTransients <- function(pset) chStrings({
  chPropset(pset)
  infos <- pset@infos
  purrr::keep(pset@props, function(p) !(infos[[p]]@transient))
})

#' @return PropInfo
#' @export
propInfo <- function(prop, pset) {
  info <- pset@infos[[prop]]
  if (is.null(info)) stop(paste0("Unrecognized prop ", prop))
  info
}

#' @return :chFmt
#' @export
propFmt <- function(prop, pset) propInfo(prop, pset)@fmt

#' @return :chBool
#' @export
propTransient <- function(prop, pset) pset@infos[[prop]]@transient

#' @export
propsetPropsOfFmt <- function(pset, fmt, props = NULL) chStrings({
  chPropset(pset)
  chFmt    (fmt)
  chMaybe  (chStrings, props)

  infos <- pset@infos
  purrr::keep(props %or% pset@props, function(p) {
    info <- infos[[p]]
    !is.null(info) && identical(fmt, info@fmt)
  })
})

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

propsetDTfmt <- function(dt, pset, f, props, fmt, suppWgsFor, ...) { # chDT
  chDT     (dt)
  chPropset(pset)
  chMaybe  (chStrings, props)
  chMaybe  (chFmt, fmt)
  chStrings(suppWgsFor)

  props <- props %or% pset@props

  # Skip missing props
  if (is.null(fmt))
    props <- props[props %in% colnames(dt) & props %in% pset@props]
  else
    # When fmt is given explicitly, there is no need to check if a prop
    # is in pset@props - we don't need this invariant
    props <- props[props %in% colnames(dt)]

  for (p in props)
    tryCatch({
      pfmt <- fmt %or% propFmt(p, pset)
      if (pfmt@ident)
        pfmt@ch(dt[[p]]) # For identities only make a check
      else
        koR::setDT(dt, p, f(pfmt, dt[[p]], ...))
    }, error = function(e) {
      stop("Error(s) fmt'ing prop ", p, ": ", e)
    }, warning = function(w) {
      if (!(p %in% suppWgsFor))
        base::warning("Warnings(s) fmt'ing prop ", p, ": ", w)
    })

  dt
}

#' @return :chDT
#' @export
propsetDT2Strings <- function(
  dt, pset, props = NULL, fmt = NULL, suppWgsFor = character(0), ...) propsetDTfmt(
    dt         = dt,
    pset       = pset,
    f          = fmt2Strings,
    props      = props,
    fmt        = fmt,
    suppWgsFor = suppWgsFor, ...)

#' @return :chDT
#' @export
propsetDTFromStrings <- function(
  dt, pset, props = NULL, fmt = NULL, suppWgsFor = character(0), ...) propsetDTfmt(
    dt         = dt,
    pset       = pset,
    f          = fmtFromStrings,
    props      = props,
    fmt        = fmt,
    suppWgsFor = suppWgsFor, ...)

#' @return :chDT
#' @export
propsetDT2Uxs <- function(
  dt, pset, props = NULL, fmt = NULL, suppWgsFor = character(0), ...) propsetDTfmt(
    dt         = dt,
    pset       = pset,
    f          = fmt2Uxs,
    props      = props,
    fmt        = fmt,
    suppWgsFor = suppWgsFor, ...)

#' @return :chDT
#' @export
propsetDTFromUxs <- function(
  dt, pset, props = NULL, fmt = NULL, suppWgsFor = character(0), ...) propsetDTfmt(
    dt         = dt,
    pset       = pset,
    f          = fmtFromUxs,
    props      = props,
    fmt        = fmt,
    suppWgsFor = suppWgsFor, ...)

#' @return :chDT
#' @export
propsetDTcoerce <- function(
  dt, pset, props = NULL, fmt = NULL, suppWgsFor = character(0), ...) propsetDTfmt(
    dt         = dt,
    pset       = pset,
    f          = fmtCoerce,
    props      = props,
    fmt        = fmt,
    suppWgsFor = suppWgsFor, ...)

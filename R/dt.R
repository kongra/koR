# Copyright (c) Konrad Grzanek
# Created 2015-07-20
#

# NON-DESTRUCTIVES
#

#' @param x chDT|chR(chDT)|chV(chDT)
#' @return chDT
#' @export
asDT <- function(x) if (is.data.table(x)) x else chDT(x@deref)

#' @param x chDT|chR(chDT)
#' @return chDT
#' @export
asDTmut <- function(x) {
  if (is.data.table(x))
    x
  else if (isR(x))
    chDT(x@deref)
  else
    stop("Only chDT or chR(chDT) are supported,",
         chR::errMessage(x))
}

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @return chVector
#' @export
getDT <- function(dt, prop) {
  chString(prop)
  asDT(dt)[[prop]]
}

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @return chBool
#' @export
hasDTprops <- function(dt, props) {
  dt <- asDT(dt)
  chStrings(props)
  all(props %in% colnames(dt))
}

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @return the dt argument
#' @export
assertDTprops <- function(dt, props, checkDups = TRUE) {
  chStrings(props)
  chBool   (checkDups)
  orig <- dt

  if (checkDups) {
    dups <- props[duplicated(props)]
    if (length(dups) != 0L)
      stop("props contains duplicated value(s) ", dups)
  }

  dt       <- asDT(dt)
  colNames <- colnames(dt)

  if (checkDups) {
    dups <- colNames[duplicated(colNames)]
    if (length(dups) != 0L)
      stop("dt contains duplicated column(s) ", dups)
  }

  diff1 <- colNames[!(colNames %in% props)] # setdiff(colNames, props)
  diff2 <- props   [!(props %in% colNames)] # setdiff(props, colNames)

  if (length(diff1) != 0L) stop("colnames(dt) contains unrecognized column(s) ", diff1)
  if (length(diff2) != 0L) stop("colnames(dt) lacks required column(s) "       , diff2)

  orig
}

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
writeDTexcel <- function(dt, name, sheetName = "Data") chUnit({
  dt <- asDT(dt)
  chString(name)
  chString(sheetName)

  fileName <- paste0(trimws(name), ".xlsx")
  if (nrow(dt) == 0L) {
    cat("No data for file", fileName, ", skipping\n")
  } else {
    cat("Writing file", fileName, "\n")
    wb    <- openxlsx::createWorkbook()
    sheet <- openxlsx::addWorksheet(wb, sheetName)
    openxlsx::writeData(wb, sheet, dt)
    openxlsx::saveWorkbook(wb, fileName, overwrite = TRUE)
  }
  NULL
})

# CONSTRUCTORS/ITERATORS
#

#' @export
bindDTs <- function(..., fill = FALSE) chDT({
  chBool(fill)
  rbindlist(purrr::map(list(...), asDT), fill = fill)
})

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
mapDT <- function(dt, f, fill = FALSE) chDT({
  dt <- asDT(dt)
  chFun (f)
  chBool(fill)
  rows <- by(dt, seq_len(nrow(dt)), f)
  rbindlist(rows, fill = fill)
})

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
forDT <- function(dt, f) chUnit({
  dt <- asDT(dt)
  chFun(f)
  by(dt, seq_len(nrow(dt)), f)
  NULL
})

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
reduceDTprops <- function(dt, props, f, ...) {
  dt <- asDT(f)
  chStrings(props)
  chFun    (f)
  purrr::reduce(as.list(props), function(x, c) f(x, dt[[c]]), ...)
}

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
withDTprops <- function(dt, props) chDT({
  chStrings(props)
  asDT(dt)[, ..props]
})

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
withoutDTprops <- function(dt, props) chDT({
  dt <- asDT(dt)
  chStrings(props)

  colNames <- colnames(dt)
  if (!all(props %in% colNames)) stop("all props must be in colnames(dt)")
  props <- setdiff(colNames, props)
  dt[, ..props]
})

getDTpropsMatching <- function(dt, pred, quant = any) chStrings({
  dt <- asDT(dt)
  chFun(pred)
  chFun(quant)
  purrr::keep(. = colnames(dt), .p = function(p) quant(pred(dt[[p]])))
})

# DESTRUCTIVE
#

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
setDT <- function(dt, i = NULL, j, v) {
  data.table::set(x = asDTmut(dt), i = i, j = j, value = v)
  dt
}

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
overDT <- function(dt, j, f, ...) {
  orig <- dt
  dt   <- asDTmut(dt)
  data.table::set(x = dt, j = j, value = f(dt[[j]], ...))
  orig
}

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
setDTkey <- function(dt, ...) {
  setkey(asDTmut(dt), ...)
  dt
}

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
setDTprops <- function(dt, old, new) {
  chStrings(old)
  chStrings(new)
  setnames(x = asDTmut(dt), old = old, new = new)
  dt
}

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
delDTprops <- function(dt, props) {
  orig <- dt
  dt   <- asDTmut(dt)
  chStrings(props)
  for (p in props) data.table::set(x = dt, j = p, value = NULL)
  orig
}

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
setDTpropsorder <- function(dt, neworder) {
  orig <- dt
  dt   <- asDTmut(dt)
  chStrings  (neworder)
  setcolorder(dt, neworder)
  orig
}

#' Diagnostic version of \code{setDTpropsorder}
#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
setDTpropsorder__ <- function(dt, neworder) {
  orig <- dt
  dt   <- asDTmut(dt)
  chStrings    (neworder)
  assertDTprops(dt, neworder)
  setcolorder  (dt, neworder)
  orig
}

#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
moveDTprops <- function(dt, ...) {
  orig     <- dt
  dt       <- asDTmut(dt)
  neworder <- moveNames(colnames(dt), ...)
  setcolorder(dt, neworder)
  orig
}

#' Diagnostic version of \code{moveDTprops}
#' @param dt chDT|chR(chDT)
#' @return the dt argument
#' @export
moveDTprops__ <- function(dt, ...) {
  orig     <- dt
  dt       <- asDTmut(dt)
  neworder <- moveNames(colnames(dt), ...)
  assertDTprops(dt, neworder)
  setcolorder  (dt, neworder)
  orig
}

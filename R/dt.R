# Copyright (c) Konrad Grzanek
# Created 2015-07-20
#

# NON-DESTRUCTIVES
#

#' Alias for \code{.subset2}
#' @export
getProp <- .subset2

#' @return chBool
#' @export
hasDTprops <- function(dt, props) {
  chDT     (dt)
  chStrings(props)
  all(props %in% colnames(dt))
}

#' @return chDT
#' @export
assertDTprops <- function(dt, props, checkDups = TRUE) {
  chDT     (dt)
  chStrings(props)
  chBool   (checkDups)

  if (checkDups) {
    dups <- props[duplicated(props)]
    if (length(dups) != 0L)
      stop("props contains duplicated value(s) ", dups)
  }

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

  dt
}

#' @return chUnit/NULL
#' @export
writeDTexcel <- function(dt, name, sheetName = "Data") {
  chDT    (dt)
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
}

# CONSTRUCTORS/ITERATORS
#

#' @return chDT
#' @export
bindDTs <- function(..., fill = FALSE) {
  chBool(fill)
  rbindlist(purrr::map(list(...), chDT), fill = fill)
}

#' @return chDT
#' @export
mapDT <- function(dt, f, fill = FALSE) {
  chDT  (dt)
  chFun (f)
  chBool(fill)
  rows <- by(dt, seq_len(nrow(dt)), f)
  rbindlist(rows, fill = fill)
}

#' @return chUnit/NULL
#' @export
forDT <- function(dt, f) {
  chDT (dt)
  chFun(f)
  by(dt, seq_len(nrow(dt)), f)
  NULL
}

#' @export
reduceDTprops <- function(dt, props, f, ...) {
  chDT     (dt)
  chStrings(props)
  chFun    (f)
  purrr::reduce(as.list(props), function(x, p) f(x, .subset2(dt, p)), ...)
}

#' @return chDT
#' @export
withDTprops <- function(dt, props) {
  chStrings(props)
  dt[, ..props]
}

#' @return chDT
#' @export
withoutDTprops <- function(dt, props) {
  chStrings(props)
  colNames <- colnames(dt)
  props    <- colNames[!(colNames %in% props)] # setdiff(colNames, props)
  dt[, ..props]
}

#' @return chStrings
#' @export
getDTpropsMatching <- function(dt, pred, quant = any) {
  chDT (dt)
  chFun(pred)
  chFun(quant)
  purrr::keep(. = colnames(dt), .p = function(p) quant(pred(.subset2(dt, p))))
}

# DESTRUCTIVE
#

#' @return chDT
#' @export
setDT <- function(dt, j, v) {
  data.table::set(x = dt, j = j, value = v)
  dt
}

#' @return chDT
#' @export
overDT <- function(dt, j, f, ...) {
  data.table::set(x = dt, j = j, value = f(.subset2(dt, j), ...))
  dt
}

#' @return chDT
#' @export
setDTkey <- function(dt, ...) {
  data.table::setkey(dt, ...)
  dt
}

#' @return chDT
#' @export
setDTprops <- function(dt, old, new) {
  chDT     (dt)
  chStrings(old)
  chStrings(new)
  setnames (x = dt, old = old, new = new)
  dt
}

#' @return chDT
#' @export
delDTprops <- function(dt, props) {
  chStrings(props)
  for (p in props) data.table::set(x = dt, j = p, value = NULL)
  dt
}

#' @return chDT
#' @export
setDTpropsorder <- function(dt, neworder) {
  chDT     (dt)
  chStrings(neworder)
  setcolorder(dt, neworder)
  dt
}

#' Diagnostic version of \code{setDTpropsorder}
#' @return chDT
#' @export
setDTpropsorder__ <- function(dt, neworder) {
  chDT     (dt)
  chStrings(neworder)
  assertDTprops(dt, neworder)
  setcolorder(dt, neworder)
  dt
}

#' @return chDT
#' @export
moveDTprops <- function(dt, ...) {
  data.table::setcolorder(dt, moveNames(colnames(dt), ...))
  dt
}

#' Diagnostic version of \code{moveDTprops}
#' @return chDT
#' @export
moveDTprops__ <- function(dt, ...) {
  neworder <- moveNames(colnames(dt), ...)
  assertDTprops(dt, neworder)
  data.table::setcolorder(dt, neworder)
  dt
}

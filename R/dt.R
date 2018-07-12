# Copyright (c) Konrad Grzanek
# Created 2015-07-20
#

# hasDTcols         -> hasDTprops (sprawdzić, jak jest używany)
# assertDTcols      -> assertDTprops
# writeReportFile   -> writeDTexcel
# reduceDTcols      -> reduceDTprops
# withDTcols        -> withoutDTprops
# withoutDTcols     -> withoutDTprops
# getDTcolsMatching -> getDTpropsMatching

# NON-DESTRUCTIVES
#

#' @param x chDT|chR(chDT)|chV(chDT)
#' @export
asDT <- function(x) if (is.data.table(x)) x else chDT(x@deref)

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @return chVector
#' @export
getDT <- function(dt, prop) {
  chString(prop)
  asDT(dt)[[prop]]
}

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @export
hasDTprops <- function(dt, props) chBool({
  dt <- asDT(dt)
  chStrings(props)
  all(props %in% colnames(dt))
})

#' @param dt chDT|chR(chDT)|chV(chDT)
#' @return dt argument
#' @export
assertDTprops <- function(dt, props) {
  chStrings(props)
  dups <- props[duplicated(props)]
  if (length(dups) != 0)
    stop("props contains duplicated value(s) ", dups)

  orig     <- dt # The original dt argument ...
  dt       <- asDT(dt)
  colNames <- colnames(dt)
  dups     <- colNames[duplicated(colNames)]
  if (length(dups) != 0)
    stop("dt contains duplicated column(s) ", dups)

  diff1 <- setdiff(colNames, props)
  diff2 <- setdiff(props, colNames)

  if (length(diff1) != 0) stop("colnames(dt) contains unrecognized column(s) ", diff1)
  if (length(diff2) != 0) stop("colnames(dt) lacks required column(s) "       , diff2)

  # ... is returned untouched
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
reduceDTprops <- function(dt, props, f, ...) chVector({
  dt <- asDT(f)
  chStrings(props)
  chFun    (f)
  purrr::reduce(as.list(props), function(x, c) f(x, dt[[c]]), ...)
})

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

# library(data.table)
# library(microbenchmark)
# library(chR)
# library(koR)
# library(purrr)
#
# dt1 <- data.table(x = 1L)
# r1  <- asR(dt1)

# DESTRUCTIVE
#
# setDT
# overDT
# delDTprops
# moveDTprops     (moveDTcols)
# setDTpropsorder (setDTcolorder)
# setDTnames
# setDTkey

#' Sets columns ordering in dt
#' @export
setDTcolorder <- function(dt, neworder) chDT({
  chDT        (dt)
  chStrings   (neworder)
  assertDTcols(dt, neworder)
  setcolorder (dt, neworder)
})

#' Uses \code{moveNames} to set a new column ordering (destructive on dt)
#' @export
moveDTcols <- function(dt, ...) chDT({
  chDT(dt)
  setDTcolorder(dt, moveNames(colnames(dt), ...))
  dt
})

#' Sets data.table's column value to value and returns the data.table.
#' @param dt a data.table
#' @param col a column of dt
#' @param value to set
#' @return dt
#' @export
setDT <- function(dt, col, value) {
  data.table::set(x = dt, j = col, value = value)
  dt
}

#' Sets data.table's new value of a column to a value of a function call
#' with an old value of the column. Does not check if column exists in dt.
#' @param dt a data.table
#' @param col a column of dt
#' @param f a function
#' @param ... additional arguments to f call
#' @return dt
#' @export
overDT <- function(dt, col, f, ...) {
  setDT(dt, col, f(dt[[col]], ...))
}

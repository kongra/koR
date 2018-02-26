# Copyright (c) Konrad Grzanek
# Created 2015-07-20
#

#' @import data.table
#' @import assertthat
#' @import chR
NULL

#' Binds the data.table arguments into one data.table, optionally filling
#' @return the resulting data.table
#' @export
bindDTs <- function(..., fill = FALSE) chDT({
  chBool(fill)
  result <- NULL
  for (dt in list(...)) {
    chDT(dt)
    if (is.null(result))
      result <- dt
    else
      result <- rbindlist(list(result, dt), fill = fill)
  }
  result
})

#' Maps f over each singleton data.tables representing the rows of dt
#' @param dt chDT
#' @param f chFun chDT1 → chDT
#' @param fill a Bool
#' @return chDT
#' @export
mapDT <- function(dt, f, fill = FALSE) chDT({
  chDT(dt)
  chFun(f)
  chBool(fill)
  result <- data.table()
  by(dt, seq_len(nrow(dt)), function (row) {
    result <<- rbindlist(list(result, chDT(f(row))), fill = fill)
  })
  result
})

#' Executes f for every row of dt
#' @export
forDT <- function(dt, f) chUnit({
  chDT(dt)
  chFun(f)
  by(dt, seq_len(nrow(dt)), f)
  NULL
})

#' @export
reduceDTcols <- function(dt, cols, f, ...) chVector({
  chDT(dt)
  chStrings(cols)
  chFun(f)
  purrr::reduce(as.list(cols), function(x, c) f(x, dt[[c]]), ...)
})

#'Thanks to: https://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
#' @export
moveNames <- function(names, toMove, pos = "last", what = NULL) chStrings({
  chStrings(names)
  chStrings(toMove)
  chString (pos)
  chMaybe  (chString, what)

  assert_that(all(toMove %in% names))

  temp <- setdiff(names, toMove)
  switch(pos,
         first = c(toMove, temp),
         last  = c(temp, toMove),
         before = {
           if (is.null(what)) stop("what must be specified when using `before`")
           assert_that(what %in% names)
           append(temp, values = toMove, after = (match(what, temp)-1))
         },
         after = {
           if (is.null(what)) stop("what must be specified when using `after`")
           assert_that(what %in% names)
           append(temp, values = toMove, after = (match(what, temp)))
         })
})

#' Takes a vector of Strings and returns a new vector with a new String s2 moving
#' it onto a pos related to s1.
#' @param s a vector of Strings
#' @param s2 a new String
#' @param pos see \code{moveNames}
#' @param s1 an element in s according to which we position s2
#' @return a new vector of Strings with s2 pos(itioned) according to s1
#' @export
withStr <- function(s, s2, pos = "after", s1) chStrings({
  chStrings(s)
  chString (s1)
  chString (s2)
  moveNames(c(s, s2), s2, pos, s1)
})

#' Asserts that colnames(dt) equals cols
#' @export
assertDTcols <- function(dt, cols) chDT({
  chDT     (dt)
  chStrings(cols)

  colNames <- colnames(dt)

  dups <- colNames[duplicated(colNames)]
  if (length(dups) != 0) stop("dt contains duplicated column(s) ", dups)

  dups <- cols[duplicated(cols)]
  if (length(dups) != 0) stop("cols contain duplicated value(s) ", dups)

  diff1 <- setdiff(colNames, cols)
  diff2 <- setdiff(cols, colNames)

  if (length(diff1) != 0) stop("colnames(dt) contain unrecognized column(s) ", diff1)
  if (length(diff2) != 0) stop("colnames(dt) lack required column(s) "       , diff2)

  dt
})

#' Checks if cols are valid colnames(dt)
#' @export
hasDTcols <- function(dt, cols) chBools({
  chDT     (dt)
  chStrings(cols)
  all(cols %in% colnames(dt))
})

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

#' Builds a data.table by using only cols of dt
#' @param dt a data.table
#' @param cols a vector of Strings
#' @export
withDTcols <- function(dt, cols) chDT({
  chDT(dt)
  chStrings(cols)
  assert_that(all(cols %in% colnames(dt)))
  dt[, ..cols]
})

#' Builds a data.table by removing cols of dt
#' @param dt a data.table
#' @param cols a vector of Strings
#' @export
withoutDTcols <- function(dt, cols) chDT({
  chDT(dt)
  chStrings(cols)

  colNames <- colnames(dt)
  assert_that(all(cols %in% colNames))
  cols <- setdiff(colNames, cols)
  dt[, ..cols]
})

#' Returns a set of dt columns c such that match predicate pred on values.
#' @param dt a data.table
#' @param pred a function
#' @param quant rows-related quantifier function, e.g. any, every
#' @return a resulting set of columns
#' @export
getDTcolsMatching <- function(dt, pred, quant = any) chStrings({
  chDT (dt)
  chFun(pred)
  chFun(quant)
  purrr::keep(. = colnames(dt), .p = function(c) {
    vals <- dt[, get(c)]
    quant(pred(vals))
  })
})

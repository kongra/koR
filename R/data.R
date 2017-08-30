# Copyright (c) Konrad Grzanek
# Created 2015-07-20

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
#' @return chDT
#' @export
mapDT <- function(dt, f) chDT({
  chDT(dt)
  chFun(f)
  result <- data.table()
  by(dt, seq_len(nrow(dt)), function (row) {
    result <<- rbindlist(list(result, chDT(f(row))), fill = TRUE)
  })
  result
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
moveNames <- function(names, tomove, pos = "last", what = NULL) chStrings({
  chStrings(names)
  chStrings(tomove)
  chString (pos)
  chMaybe  (chString, what)

  assert_that(all(tomove %in% names))

  temp <- setdiff(names, tomove)
  switch(pos,
         first = c(tomove, temp),
         last  = c(temp, tomove),
         before = {
           if (is.null(what)) stop("what must be specified when using `before`")
           assert_that(what %in% names)
           append(temp, values = tomove, after = (match(what, temp)-1))
         },
         after = {
           if (is.null(what)) stop("what must be specified when using `after`")
           assert_that(what %in% names)
           append(temp, values = tomove, after = (match(what, temp)))
         })
})

#' Uses \code{moveNames} to set a new column ordering (destructive on dt)
#' @export
moveDTcols <- function(dt, ...) chDT({
  chDT(dt)
  setcolorder(dt, moveNames(colnames(dt), ...))
  dt
})

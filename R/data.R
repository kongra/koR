# Copyright (c) Konrad Grzanek
# Created 2015-07-20

#' @import data.table
#' @import purrr
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

chMapDTRow <- chOr(chDT01, chUnit, chNA)

#' Maps f over each singleton data.tables representing the rows of dt
#' @param dt chDT
#' @param f chFun chDT1 chDT1
#' @return chDT
#' @export
mapDT <- function(dt, f) chDT({
  chDT(dt)
  chFun(f)
  result <- data.table()
  by(dt, seq_len(nrow(dt)), function (rowDT) {
    result <<- rbindlist(list(result, rowDT %>% f %>% chMapDTRow), fill = TRUE)
  })
  result
})

# Copyright (c) Konrad Grzanek
# Created 2015-07-20

#' Report matching column values counts.
#'
#' \code{printColsCounts} prints counts of items in data frame columns that
#' match the given predicate.
#' @param df The data frame
#' @param pred The predicate
#' @param printZeros When TRUE the zero counts will be printed out
#' @return NULL
#' @export
printColsCounts <- function(df, pred, printZeros = FALSE) {
  predName <- as.character(substitute(pred))
  for (n in colnames(df)) {
    v <- sum(pred(df[[n]]))
    if (printZeros || v != 0) {
      cat(paste(n, "has", v, "matching", predName, "\n"))
    }
  }
}

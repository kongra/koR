# Copyright (c) Konrad Grzanek
# Created 2017-06-02

#' @import chR
NULL

#' Returns a timestamp string with the tzone appended.
#' @export
stampString <- function(tzone = "CEST") chString({
  chString(tzone)
  paste0(format.POSIXct(Sys.time()), " ", tzone)
})


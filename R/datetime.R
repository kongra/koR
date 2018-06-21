# Copyright (c) Konrad Grzanek
# Created 2017-06-02
#

#' Returns a timestamp string with the tzone appended.
#' @export
stampString <- function(tzone = "CEST", hsep = "_") chString({
  chString(tzone)
  chString(hsep)
  s <- paste0(format.POSIXct(Sys.time()), " ", tzone)
  if (hsep == ":") s else str_replace_all(s, ":", hsep)
})


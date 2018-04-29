# Copyright (c) Konrad Grzanek
# Created 2017-09-17 20_30_24 CEST
#

#' @import chR
NULL

#' Ensures that either file argument or its archived version exist
#' @param file path/to/some/file.ext
#' @return path/to/some/file.ext or archive/path/to/some/file.ext
#' @export
ensureF <- function(file, archFile = NULL) chString({
  chString(file)
  chMaybe(chString, archFile)

  if (file.exists(file))
    file
  else {
    if (is.null(archFile)) archFile <- paste0("archive/", file)
    if (file.exists(archFile))
      archFile
    else
      stop(paste0("Neither of the files exist: ", file, ", ", archFile))
  }
})

#' Moves the file to a proper archive/ subdir
#' @param file path/to/some/file.ext
#' @return archive/path/to/some/file.ext
#' @export
archF <- function(file) chString({
  chString(file)
  archFile <- paste0("archive/", file)
  file     <- ensureF(file, archFile)
  if (file != archFile) { # otherwise already in archive/
    archDir <- dirname(archFile)
    if (!dir.exists(archDir )) dir.create (archDir)
    if (file.exists(archFile)) file.remove(archFile)
    file.rename(file, archFile)
  }
  archFile
})

# Copyright (c) Konrad Grzanek
# Created 2018-07-25
#

#' A fast memoizer for values of depending on some logical condition.
#' @export
memoBool <- function(forTrue, forFalse) {
  cache <- list(forTrue, forFalse)
  function(b) .subset2(cache, if (b) 1L else 2L)
}

CAPT_ARGS <- new.env(hash = TRUE, parent = emptyenv())

procCaptArg                <- function(x) UseMethod("procCaptArg")
procCaptArg.default        <- base::identity
procCaptArg.reactivevalues <- shiny::reactiveValuesToList

#' Captures the arguments for calling f. When called without f returns
#' the last captured args for given id.
#' @export
captArgs <- function(id, f = NULL, ...) {
  chString(id)
  chMaybe (chFun, f)

  if (is.null(f))
    .subset2(CAPT_ARGS, id)
  else {
    CAPT_ARGS[[id]] <- purrr::map(list(...), procCaptArg)
    f(...)
  }
}

#' Invokes f with the last captured arguments.
#' @seealso \code{captArgs}
#' @export
callCaptArgs <- function(id, f) {
  chString(id)
  chFun   (f)
  do.call(f, .subset2(CAPT_ARGS, id))
}

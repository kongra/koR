# Copyright (c) Konrad Grzanek
# Created 2018-07-25
#

#' A fast memoizer for values of depending on some logical condition.
#' @export
memoBool <- function(forTrue, forFalse) {
  cache <- list(forTrue, forFalse)
  function(b) .subset2(cache, if (b) 1L else 2L)
}

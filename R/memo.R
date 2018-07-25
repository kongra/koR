# Copyright (c) Konrad Grzanek
# Created 2018-07-25
#

#' A fast memoizer for values of function call depending on some logical
#' condition.
#' @export
memoBool <- function(f, ...) {
  chFun(f)
  cache <- list(NULL, NULL)
  function(b) {
    idx <- if (b) 1L else 2L
    v   <- cache[[idx]]
    if (!is.null(v))
      v
    else {
      v <- f(...)
      cache[[idx]] <<- v
      v
    }
  }
}

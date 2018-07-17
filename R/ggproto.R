function (`_class` = NULL, `_inherit` = NULL, ...)
{
  e <- new.env(parent = emptyenv())
  members <- list(...)
  if (length(members) != sum(nzchar(names(members)))) {
    stop("All members of a ggproto object must be named.")
  }
  if (length(members) > 0) {
    list2env(members, envir = e)
  }
  `_inherit` <- substitute(`_inherit`)
  env <- parent.frame()
  find_super <- function() {
    eval(`_inherit`, env, NULL)
  }
  super <- find_super()
  if (!is.null(super)) {
    if (!is.ggproto(super)) {
      stop("`_inherit` must be a ggproto object.")
    }
    e$super <- find_super
    class(e) <- c(`_class`, class(super))
  }
  else {
    class(e) <- c(`_class`, "ggproto", "gg")
  }
  e
}

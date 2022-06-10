null.omit <- function(l) l[lengths(l) > 0]

`%||%` <- function(x, y) if (is.null(x)) y else x

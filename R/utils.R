null.omit <- function(l) l[lengths(l) > 0]

`%||%` <- function(x, y) if (is.null(x)) y else x

interval_mins <- function(g, o = lubridate::origin) lubridate::interval(o, g, tzone = "UTC") %>% lubridate::time_length(unit = "minutes")

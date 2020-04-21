
.fortnight <- function(x, ndays = 14) ((lubridate::yday(x) - 1) %/% ndays) + 1

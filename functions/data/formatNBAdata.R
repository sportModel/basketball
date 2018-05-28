formatNBAdata <- function() {
  out <- NULL
  pb <- txtProgressBar(0, length(par@team), style=3)
  counter <- 0
  for (i in par@team) {
    counter <- counter + 1
    out <- rbind(out, formatNBAplayer(i))
    setTxtProgressBar(pb, counter)
  }
  out
}

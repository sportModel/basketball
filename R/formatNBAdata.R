formatNBAdata <- function() {
  out <- NULL
  pb <- txtProgressBar(0, length(par@team), style=3)
  counter <- 0
  for (team in par@team) {
    counter <- counter + 1
    out <- rbind(out, formatNBAplayer(team))
    setTxtProgressBar(pb, counter)
  }
  out
}

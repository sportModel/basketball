formatNBAteamData <- function(standings) {
  out <- NULL
  for (i in par@team) {
    out <- rbind(out, formatNBAteam(i, standings))
  }
  setkey(out, Team)
  out
}

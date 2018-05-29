formatNBAteamData <- function(standings) {
  out <- NULL
  for (team in par@team) {
    out <- rbind(out, formatNBAteam(team, standings))
  }
  setkey(out, Team)
  out
}

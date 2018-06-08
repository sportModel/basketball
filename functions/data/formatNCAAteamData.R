formatNCAAteamData <- function() {
  out <- NULL
  for (team in par@team) {
    out <- rbind(out, formatNCAAteam(team))
  }
  setkey(out, Team)
  out
}

formatNCAAteamData <- function() {
  team.raw <- NULL
  for (i in par@team) {
    team.i <- formatNCAAteam(i)
    team.raw <- rbind(team.raw, team.i)
  }
  rownames(team.raw) <- par@team
  TmORBPct <- team.raw[,"TmORB"]/(team.raw[,"TmORB"]+team.raw[,"OpDRB"])
  OpORBPct <- team.raw[,"OpORB"]/(team.raw[,"OpORB"]+team.raw[,"TmDRB"])
  TmPoss <- team.raw[,"TmFGA"]-TmORBPct*(team.raw[,"TmFGA"]-team.raw[,"TmFG"])*par@constants[1]+team.raw[,"TmTOV"]+team.raw[,"TmFTA"]*par@constants[2]
  OpPoss <- team.raw[,"OpFGA"]-OpORBPct*(team.raw[,"OpFGA"]-team.raw[,"OpFG"])*par@constants[1]+team.raw[,"OpTOV"]+team.raw[,"OpFTA"]*par@constants[2]
  team.raw <- as.data.frame(team.raw)
  team.raw$Poss <- (TmPoss+OpPoss)/2/team.raw[,"TmG"]
  team.raw
}

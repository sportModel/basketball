formatNBAdata <- function() {
  roster <- totals <- advanced <- team.opp <- NULL
  pb <- txtProgressBar(0, length(par@team), style=3)
  counter <- 0
  for (i in par@team) {
    counter <- counter + 1
    standings <- formatNBAstandings()
    teamData <- formatNBAteam(i,standings)
    roster <- rbind(roster,teamData$roster)
    totals <- rbind(totals,teamData$totals)
    advanced <- rbind(advanced,teamData$advanced)
    team.opp <- rbind(team.opp,teamData$team.opp)
    setTxtProgressBar(pb, counter)
  }
  val <- list(roster,totals,advanced,team.opp,standings)
  names(val) <- c("Roster","Totals","Advanced","Team.Opp","Standings")
  return(val)
}

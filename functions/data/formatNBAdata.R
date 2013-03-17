formatNBAdata <- function()
  {
    roster <- totals <- advanced <- team.opp <- NULL
    for (i in par@team)
      {
        standings <- formatNBAstandings()
        teamData <- formatNBAteam(i,standings)
        roster <- rbind(roster,teamData$roster)
        totals <- rbind(totals,teamData$totals)
        advanced <- rbind(advanced,teamData$advanced)
        team.opp <- rbind(team.opp,teamData$team.opp)
      }
    val <- list(roster,totals,advanced,team.opp,standings)
    names(val) <- c("Roster","Totals","Advanced","Team.Opp","Standings")
    return(val)
  }

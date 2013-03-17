formatNCAAteamData <- function()
  {
    team <- NULL
    for (i in par@team)
      {
        team.i <- formatNCAAteam(i)
        team <- rbind(team,team.i)
      }
    rownames(team) <- par@team
    return(team)
  }

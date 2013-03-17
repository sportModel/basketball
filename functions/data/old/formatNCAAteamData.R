formatNCAAteamData <- function()
  {
    val <- NULL
    for (i in par@team)
      {
        val <- rbind(val,formatNCAAteam(i))
      }
    rownames(val) <- par@team
    return(val)
  }

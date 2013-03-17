formatNBAteamData <- function()
  {
    val <- NULL
    for (i in par@team)
      {
        val <- rbind(val,formatNBAteam(i))
      }
    rownames(val) <- par@team
    return(val)
  }

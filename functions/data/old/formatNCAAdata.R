formatNCAAdata <- function()
  {
    val <- NULL
    for (i in ncaa.par@team)
      {
        val.i <- formatNCAAteamStats(ncaa.par@year,i)
        val <- rbind(val,val.i)
      }
    ind <- order(apply(val[,1:2],1,paste,collapse=" "))
    val[ind,]
  }

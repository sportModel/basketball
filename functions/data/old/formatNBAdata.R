formatNBAdata <- function()
  {
    val <- NULL
    for (i in nba.par@team)
      {
        stats <- formatNBAteamStats(nba.par@year,i)
        roster <- formatNBAteamRoster(nba.par@year,i)
        ind <- match(apply(stats[,1:2],1,paste,collapse=""),apply(roster[,1:2],1,paste,collapse=""))
        val.i <- cbind(roster[ind,-1:-3],stats)
        val <- rbind(val,val.i[c(8:10,1:7,11:30)])
      }
    ind.NA <- which(is.na(val[,4]))
    val.noNA <- val[!is.na(val[,4]),]
    val[ind.NA,5:10] <- val.noNA[match(apply(val[ind.NA,1:2],1,paste,collapse=""),apply(val.noNA[,1:2],1,paste,collapse="")),5:10]
    ind <- order(apply(val[,1:2],1,paste,collapse=" "))
    val[ind,]
  }

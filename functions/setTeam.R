setTeam <- function(level,year)
  {
    files <- list.files(paste("data/",level,"/",year,"/raw",sep=""))
    teams <- gsub("_.*","",files)
    teams <- gsub(".html","",teams)
    teams <- setdiff(teams,c("nba","ncaa"))
    return(unique(teams))
  }

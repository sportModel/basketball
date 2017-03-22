ncaagFormat <- function(url) {
  tmp <- tempfile()
  system(paste0("wget -q ", url, " -O ", tmp))
  system(paste0("sed -i 's/<!--//g' ", tmp))
  system(paste0("sed -i 's/-->//g' ", tmp))
  raw <- readHTMLTable(tmp)
  ffind <- which(names(raw)=='four-factors')
  school <- raw[[ffind]][2:3,1]
  
  val <- list(team1=ncaagFormatTeam(raw[[ffind+1]], school[1]),
              team1=ncaagFormatTeam(raw[[ffind+3]], school[2]))
  names(val) <- school
  val
}
ncaagFormatTeam <- function(team, tname) {
  team <- team[team$Starters!="Reserves",]
  for (j in 2:ncol(team)) {
    team[,j] <- as.numeric(team[,j])
  }
  val <- data.frame(Name=team$Starters, Team=tname, Pos='', pg=.2, sg=.2, sf=.2, pf=.2, c=.2, No=NA, Yr=NA, Ht=NA, Wt=NA, G=1, team[,-1], check.names=FALSE)
}

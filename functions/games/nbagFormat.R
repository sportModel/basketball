nbagFormat <- function(url) {
  tmp <- tempfile()
  system(paste0("wget -q ", url, " -O ", tmp))
  system(paste0("sed -i 's/<!--//g' ", tmp))
  system(paste0("sed -i 's/-->//g' ", tmp))
  raw <- readHTMLTable(tmp)
  ffind <- which(names(raw)=='four_factors')
  team <- raw[[ffind]][2:3,1]

  val <- list(team1=nbagFormatTeam(raw[[ffind+1]], team[1]),
              team1=nbagFormatTeam(raw[[ffind+3]], team[2]))
  names(val) <- team
  val
}
nbagFormatTeam <- function(team, tname) {
  team <- team[team$Starters!="Reserves" & !(team$MP %in% c("Did Not Play", "Did Not Dress")),]
  for (j in 3:ncol(team)) {
    team[,j] <- as.numeric(team[,j])
  }
  team$MP <- fixMP(team$MP)
  val <- data.frame(Name=team$Starters, Team=tname, pg=.2, sg=.2, sf=.2, pf=.2, c=.2, No=NA, Yr=NA, Ht=NA, Wt=NA, G=1, team[,-1], check.names=FALSE)
}
fixMP <- function(x) {
  A <- stringr::str_split(x, ':', simplify=TRUE)
  mp <- as.numeric(A[,1]) + as.numeric(A[,2])/60
  return(round(mp, 1))
}

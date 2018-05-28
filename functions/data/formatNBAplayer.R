formatNBAplayer <- function(team) {
  pg <- xml2::read_html(paste0("data/nba/",par@year,"/raw/",team,".html"))
  if (length(pg) < 2) stop(paste0("Missing HTML file for: ", team))

  # Position
  pbp_raw <- html_node(pg, '#advanced_pbp') %>% html_table()
  X <- pbp_raw[-1,names(pbp_raw)=="Position Estimate"]
  X <- lapply(X, function(x) as.numeric(gsub('%', '', x))/100)
  X <- sapply(X, function(x) {x[which(is.na(x))] <- 0;x})
  DT <- data.table(Team=team, Name=pbp_raw[-1,2], X)
  names(DT)[3:7] <- c("pg","sg","sf","pf","c")
  setkey(DT, Name)
  
  # Totals
  totals_raw <- html_node(pg, '#totals') %>% html_table()
  totals <- data.table(Name=totals_raw[,2], totals_raw[,-(1:2)])
  setkey(totals, Name)
  DT <- merge(DT, totals, all=TRUE)
  
  # Assisted
  shooting_raw <- html_node(pg, '#shooting') %>% html_table()
  X <- shooting_raw[-(1:2), which(shooting_raw[2,]=="%Ast'd")]
  X <- lapply(X, function(x) as.numeric(x))
  X <- sapply(X, function(x) {x[is.na(x)] <- 0;x})
  shooting <- data.table(Name=shooting_raw[-(1:2),2], X)
  names(shooting)[2:3] <- c('Astd2', 'Astd3')
  setkey(shooting, Name)
  DT <- merge(DT, shooting, all=TRUE)

  # Roster
  roster_raw <- html_node(pg, '#roster') %>% html_table()
  roster <- data.table(roster_raw)[, .(No., Player, Ht, Wt, Exp)]
  setnames(roster, 'Player', 'Name')
  setkey(roster, Name)
  DT <- merge(DT, roster, all.x=TRUE)
  
  DT[Name != "Team Totals"]
}

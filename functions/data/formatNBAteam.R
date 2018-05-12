formatNBAteam <- function(team, standings) {
  pg <- xml2::read_html(paste0("data/nba/",par@year,"/raw/",team,".html"))
  if (length(pg) < 2) stop(paste0("Missing HTML file for: ", team))

  # Team name
  full.team.name <- html_nodes(pg, 'title') %>%
    stringr::str_replace(".*-[0123456789][0123456789] ", "") %>%
    stringr::str_replace(" Roster and.*", "") %>%
    stringr::str_squish()

  # Roster
  roster <- html_node(pg, '#roster') %>% html_table()
  roster <- cbind(Team=team, roster)

  # Totals
  totals <- html_node(pg, '#totals') %>% html_table()
  totals <- cbind(Team=team, Name=totals[,2], Pos="",matrix(0,ncol=6,nrow=nrow(totals)),totals[,-(1:2)])
  names(totals)[4:9] <- c("pg","sg","sf","pf","c","Astd")

  # Position
  all.files <- list.files(paste("data/nba/",par@year,"/raw",sep=""))
  team.files <- all.files[grep(paste(team,"_82_",sep=""),all.files)]
  position <- NULL
  for (player.file in team.files) {
    pg82 <- xml2::read_html(paste0("data/nba/",par@year,"/raw/",player.file))
    if (length(pg82) < 2) next
    name <- html_nodes(pg82, 'title') %>%
      stringr::str_replace("<title> ", "") %>%
      stringr::str_replace(" of the.*", "") %>%
      stringr::str_squish() %>%
      fixName()

    # Position
    raw <- html_node(pg82, '[name="bypos"] ~ table') %>% html_table()
    pct <- as.numeric(gsub('%', '', raw[-1,2]))
    pct[is.na(pct)] <- 0

    # Assisted
    raw <- html_node(pg82, 'font + table') %>% html_table()
    w <- as.numeric(gsub('%', '', raw[-1,2]))
    w <- w/sum(w)
    a <- as.numeric(gsub('%', '', raw[-1,4]))/100
    Astd <- sum(w*a)

    # Match to name
    max.distance <- 0
    repeat {
      if (max.distance > .4) {
        warning("no match for ", name, " on team ", team)
        break
      }
      ind <- agrep(name, totals$Name, max.distance=max.distance)
      if (length(ind)==1) break
      else max.distance <- max.distance + 0.1
    }

    if (sum(pct)==0) {
      totals[ind,4:8] <- .2
      totals[ind,3] <- ""
    } else {
      pct <- pct/sum(pct)
      totals[ind,4:8] <- pct
      totals[ind,3] <- names(totals[,4:8])[which.max(totals[ind,4:8])]
    }
    totals[ind,9] <- Astd
  }

  # Advanced
  advanced <- html_node(pg, '#advanced') %>% html_table()
  advanced <- data.table(Team=team, advanced)
  setnames(advanced, 'V2', 'Name')
  advanced[, Rk := NULL]
  advanced[, V18 := NULL]
  advanced[, V23 := NULL]
  
  # Team
  team.opp <- html_node(pg, '#team_and_opponent') %>% html_table()
  team.opp <- team.opp[match(c("Team","Opponent"),team.opp[,1]),-1]
  for (j in 1:ncol(team.opp)) team.opp[,j] <- as.numeric(team.opp[,j])
  team.opp <- cbind(Team=c(team,team), FullName=full.team.name, Team.Opp=c("Team","Opponent"), team.opp)
  
  # W/L
  df <- NULL
  nul <- lapply(standings,function(x){df <<- rbind(df,x)})
  w <- df$W[grep(full.team.name,rownames(df))]
  l <- df$L[grep(full.team.name,rownames(df))]
  team.opp <- cbind(team.opp,W=c(w,l),L=c(l,w))

  return(list(roster=roster,totals=totals,advanced=advanced,team.opp=team.opp))
}
fixerror <- function(x) {
  l <- length(x)
  if (substr(x[l],2,2)==".") x[l] <- substr(x[l],3,nchar(x[l]))
  paste(x,collapse=" ")
}

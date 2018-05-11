formatNBAteam <- function(team, standings) {
  tfile <- paste0("data/nba/",par@year,"/raw/",team,".html")

  # Team name
  full.team.name <- xml2::read_html(tfile) %>% html_nodes('title') %>%
    stringr::str_replace(".*-[0123456789][0123456789] ", "") %>%
    stringr::str_replace(" Roster and.*", "") %>%
    stringr::str_squish()

  # Roster
  roster <- xml2::read_html(tfile) %>% html_node('#roster') %>% html_table()
  roster <- cbind(Team=team, roster)

  # Totals
  totals <- xml2::read_html(tfile) %>% html_node('#totals') %>% html_table()
  for (i in 2:ncol(totals)) totals[,i] <- as.numeric(totals[,i])
  totals <- cbind(Team=team, Name=totals[,1], Pos="",matrix(0,ncol=6,nrow=nrow(totals)),totals[,-1])
  names(totals)[4:9] <- c("pg","sg","sf","pf","c","Astd")

  ## position
  all.files <- list.files(paste("data/nba/",par@year,"/raw",sep=""))
  team.files <- all.files[grep(paste(team,"_82_",sep=""),all.files)]
  position <- NULL
  for (player.file in team.files) {
    raw <- readLines(paste("data/nba/",par@year,"/raw/",player.file,sep=""),warn=F)
    if (length(raw)==0) next
    raw <- raw[-which(strtrim(raw,9)=="Copyright")]
    ind <- grep("size=+2",raw,fixed=TRUE)
    name <- gsub("\\<font[^\\>]*\\>","",raw[ind],perl=T)
    name <- gsub("\\</font\\>","",name,perl=T)
    name <- gsub("<br>","",name,fixed=TRUE)
    if (length(name)==0) next
    name <- fixName(name)
    ##cat(player.file,name,"\n")

    ## Position
    ind <- which(raw=="<b>Player Floor Time Stats by Position</b><br>")
    raw[ind+1] <- "<table width=540 bgcolor=cccccc border=0 cellspacing=1 id=breheny>"
    raw.pos <- extractTable(raw,"breheny")
    raw.pos <- scrub(raw.pos)
    pct <- as.numeric(matrix(raw.pos,ncol=9,byrow=T)[-1,2])
    pct[is.na(pct)] <- 0

    ## Assisted
    ind <- grep("Shot selection",raw)
    raw[ind+1] <- "<table width=290 bgcolor=cccccc border=0 cellspacing=1 id=breheny>"
    raw.ast <- extractTable(raw,"breheny")
    raw.ast <- scrub(raw.ast)
    X <- matrix(raw.ast,ncol=6,byrow=T)
    w <- as.numeric(X[2:5,2])
    w <- w/sum(w)
    a <- as.numeric(X[2:5,4])/100
    Astd <- sum(w*a)

    ## Match to name
    max.distance <- 0
    repeat {
      if (max.distance > .4) {
        warning("no match for ",name," on team ",team)
        break
      }
      ind <- agrep(name,totals$Name,max.distance=max.distance)
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

  ## advanced
  raw <- readLines(paste("data/nba/",par@year,"/raw/",team,".html",sep=""),warn=F)
  raw <- extractTable(raw,"advanced")
  raw <- scrub(raw)
  advanced <- matrix(raw,ncol=22,byrow=T)
  colnames(advanced) <- advanced[1,]
  advanced <- advanced[-1,]
  advanced <- advanced[,-1]
  advanced <- as.data.frame(advanced)
  advanced <- cbind(Team=team,advanced)

  ## Team
  raw <- readLines(paste("data/nba/",par@year,"/raw/",team,".html",sep=""),warn=F)
  raw <- extractTable(raw,"team")
  raw <- scrub(raw)
  team.opp <- matrix(raw,nrow=5,byrow=T)
  colnames(team.opp) <- team.opp[1,]
  team.opp <- team.opp[match(c("Team","Opponent"),team.opp[,1]),-1]
  team.opp <- as.data.frame(team.opp)
  for (i in 1:ncol(team.opp)) team.opp[,i] <- as.numeric(team.opp[,i])
  team.opp <- cbind(Team=c(team,team),FullName=full.team.name,Team.Opp=c("Team","Opponent"),team.opp)

  ## W/L
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

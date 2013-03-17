formatNCAAteam <- function(team)
  {
    raw <- readLines(paste("data/ncaa/",par@year,"/raw/",team,"_team.html",sep=""),warn=F)
    raw <- extractTable(raw,"tab1")
    raw <- raw[!raw==""]
    raw <- raw[!raw=="     "]
    raw <- unlist(strsplit(paste(raw,collapse=""),split="<td>"))
    raw <- gsub("\\<t[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</t[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\<a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\<d[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</d[^\\>]*\\>","",raw,perl=T)
    raw <- gsub(" ","",raw)
    tmp <- matrix(raw[-1],ncol=9,byrow=T)
    tmp <- tmp[is.element(tmp[,1],c("TotalGames","Wins","Losses","FieldGoalsMade","FieldGoalAttempts","3-ptFieldGoalsMade","3-ptFieldGoalAttempts","FreeThrowsMade","FreeThrowAttempts","OffensiveRebounds","DefensiveRebounds","Assists","Steals","Blocks","Turnovers","Points")),]
    tmp[,1] <- c("G","W","L","FG","FGA","FT","FTA","3P","3PA","PTS","ORB","DRB","AST","STL","TOV","BLK")
    val <- as.numeric(c(tmp[,2],tmp[,5]))
    names(val) <- c(paste("Tm",tmp[,1],sep=""),paste("Op",tmp[,1],sep=""))
    return(val)
  }

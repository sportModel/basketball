formatNBAteam <- function(team)
  {
    raw <- readLines(paste("data/nba/",par@year,"/raw/",team,".html",sep=""),warn=F)
    WL.line <- raw[grep("Game Results",raw)]
    WL.line <- strsplit(WL.line,"strong")[[1]][2]
    WL.line <- gsub(">","",WL.line)
    WL.line <- gsub("</","",WL.line)
    WL <- unlist(strsplit(WL.line,"-"))
    raw <- extractTable(raw,"team_and_opp")
    raw <- raw[-grep("table",raw)]
    raw <- raw[-grep("thead",raw)]
    raw <- raw[-grep("tbody",raw)]
    raw <- raw[-grep("tr",raw)]
    raw <- gsub("\\<td[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</td\\>","",raw,perl=T)
    raw <- gsub("\\<th[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</th\\>","",raw,perl=T)
    raw <- gsub(" ","",raw)
    raw <- raw[raw!=""]
    raw <- raw[raw!="Team"]
    raw <- raw[raw!="Opponent"]
    val <- as.numeric(c(raw[21],WL,raw[22:41],WL[2:1],raw[42:60]))
    raw <- c(raw[1],"W","L",raw[2:20])
    names(val) <- c(paste("Tm",raw[1:22],sep=""),paste("Op",raw[1:22],sep=""))
    return(val)
  }

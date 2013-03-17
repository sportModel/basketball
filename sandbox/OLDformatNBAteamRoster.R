OLDformatNBAteamRoster <- function(year,team)
  {
    raw <- readLines(paste("data/nba/",year,"/raw/",team,"_roster.html",sep=""),warn=F)
    tables <- grep("table",raw)
    good.table <- grep("gSGTable",raw)[1]
    ind <- match(good.table,tables)
    raw <- raw[tables[ind]:tables[ind+1]]
    raw <- gsub("\t","",raw,fixed=F)
    raw <- gsub("\\<a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</a\\>","",raw,perl=T)
    raw <- gsub("\\</td\\>","",raw,perl=T)
    raw <- gsub(" align[^\\>]*","",raw,perl=T)
    raw <- gsub(" class[^\\>]*","",raw,perl=T)
    raw <- gsub(" bgcolor[^\\>]*","",raw,perl=T)
    raw <- gsub("\\<td NOWRAP[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\<[^\\>]*td\\>","",raw,perl=T)
    raw <- gsub("\\<[^\\>]*tr\\>","",raw,perl=T)
    raw <- gsub("\\<[^\\>]*b\\>","",raw,perl=T)
    raw <- gsub("&nbsp;","",raw,fixed=T)
    raw <- gsub("- C","",raw)
    raw <- raw[-grep("!",raw)]
    raw <- raw[gsub(" ","",raw)!=""]
    raw <- raw[-grep("table",raw)]
    raw <- cut(raw[-1])
    val <- matrix(raw,ncol=8,byrow=T)
    colnames(val) <- val[1,]
    val <- val[-1,]
    val <- cbind(formatName(val[,2]),team,val[,-2])
    val
  }

formatNBAteamRoster <- function(year,team)
  {
    raw <- readLines(paste("data/nba/",year,"/raw/",team,"_roster.html",sep=""),warn=F)
    tables <- grep("table",raw)
    good.table <- grep("yspwhitebg",raw)[1]
    ind <- match(good.table,tables)
    raw <- raw[tables[ind]:tables[ind+1]]
    raw <- gsub("\\<a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</a\\>","",raw,perl=T)
    raw <- gsub("\\<td[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</td\\>","",raw,perl=T)
    raw <- gsub("\\<tr[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</tr\\>","",raw,perl=T)
    raw <- gsub("\\<span[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</span\\>","",raw,perl=T)
    raw <- gsub("\\<b[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</b\\>","",raw,perl=T)
    raw <- gsub("\\<spacer[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\<img[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\<table[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</table\\>","",raw,perl=T)
    raw <- gsub("&nbsp;","",raw)
    raw <- gsub("\\<p\\>","",raw,perl=T)
    raw <- gsub("\t","",raw,fixed=T)
    raw <- raw[raw!=""]
    raw[-grep(",",raw)] <- gsub(" ","",raw[-grep(",",raw)])
    val <- matrix(raw[-1:-3],ncol=8,byrow=T)
    colnames(val) <- val[1,]
    val <- val[-1,]
    val <- cbind(formatName(val[,2],team),team,val[,-2])
    as.data.frame(val)
  }

formatNBAteamStats <- function(year,team)
  {
    raw <- readLines(paste("data/nba/",year,"/raw/",team,"_stats.html",sep=""),warn=F)
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
    raw <- gsub("\\<table[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</table\\>","",raw,perl=T)
    raw <- gsub("&nbsp;","",raw)
    raw <- gsub("\\<p\\>","",raw,perl=T)
    raw <- raw[raw!=""]
    options(warn=-1)
    val <- matrix(raw[-1:-3],ncol=21,byrow=T)
    options(warn=0)
    colnames(val) <- val[1,]
    val <- val[-1,]
    val <- val[1:(dim(val)[1]-1),]
    val <- cbind(formatName(val[,1],team),team,val[,-1])
    colnames(val)[4:14] <- c("G","Min","M","A","Pct","M3","A3","Pct3","M1","A1","Pct1")
    val <- as.data.frame(val,stringsAsFactors=F)
    val[,"G"] <- as.numeric(val[,"G"])
    val[,"Min"] <- min2num(val[,"Min"])
    for (i in 6:(dim(val)[2])) val[,i] <- as.numeric(val[,i])
    val
  }

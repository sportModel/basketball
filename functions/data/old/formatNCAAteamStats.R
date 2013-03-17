formatNCAAteamStats <- function(year,team)
  {
    raw <- readLines(paste("data/ncaa/",year,"/raw/",team,".html",sep=""),warn=F)
    tables <- grep("table",raw)
    good.table <- grep("yspwhitebg",raw)[1]
    ind <- match(good.table,tables)
    raw <- raw[tables[ind]:tables[ind+1]]
    raw <- gsub('<td align="left" class="yspscores">&nbsp;</td>','G',raw,fixed=T)
    raw <- gsub("yspscores\"\\>\\</td","yspscores\"\\>0\\</td",raw,perl=T)
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
    val <- matrix(raw[-1],ncol=23,byrow=T)
    colnames(val) <- val[1,]
    val <- val[-1,]
    val <- cbind(formatName(val[,1],team),team,val[,-1])
    colnames(val)[4:23] <- c("Pos","G","GS","Min","M","A","Pct","M3","A3","Pct3","M1","A1","Pct1",  "Off","Def","Tot","Ast","TO","Stl","Blk")
    val <- as.data.frame(val,stringsAsFactors=F)
    for (i in 5:(dim(val)[2])) val[,i] <- as.numeric(val[,i])
    val
  }

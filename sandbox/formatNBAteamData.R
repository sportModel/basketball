formatNBAteamData <- function(year,team)
  {
    raw <- readLines(paste("data/nba/",year,"/raw/",team,"_stats.html",sep=""),warn=F)
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
    raw <- raw[-grep("colspan",raw)]
    raw <- raw[raw!="<td>&nbsp;"]
    names <- grep("&nbsp",raw)
    raw[names] <- gsub("    ","",raw[names])
    raw[names] <- gsub("&nbsp;","",raw[names])
    raw[-names] <- gsub(" ","",raw[-names])
    raw <- raw[raw!=""]
    raw <- raw[raw!="<td>"]
    raw <- raw[raw!="<tr>"]
    raw <- raw[raw!="</tr>"]
    raw <- raw[-grep("!",raw)]
    raw <- raw[-grep("/2008",raw)]
    raw <- raw[raw!="</table>"]
    raw <- raw[raw!="</div></tr>"]
    raw <- raw[-(1:6)]
    raw <- gsub("\\<tdNOWRAPwidth=..\\>\\<b\\>","",raw,perl=T)
    raw <- gsub("\\<td NOWRAP width=..\\>\\<b\\>","",raw,perl=T)
    raw <- gsub("\\<b\\>","",raw,perl=T)
    raw <- gsub("\\</b\\>","",raw,perl=T)
    raw <- gsub("/b","",raw)
    raw <- gsub("\\<","",raw,perl=T)
    val <- matrix(raw,ncol=16,byrow=T)
    colnames(val) <- val[1,]
    val <- val[-1,]
    val <- val[1:(dim(val)[1] - 2),]
    val <- cbind(formatName(val[,1]),team,val[,-1])
    colnames(val)[c(7,8,9)] <- c("Pct","Pct3","Pct1")
    val <- as.data.frame(val,stringsAsFactors=F)
    for (i in 4:(dim(val)[2])) val[,i] <- as.numeric(val[,i])
    val
  }

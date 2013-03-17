formatNCAArebs <- function(raw)
  {
    raw <- extractTable(raw,"statTab4")
    raw <- unlist(strsplit(paste(raw,collapse=""),split="<td>"))
    header <- unlist(strsplit(raw[1],split="<th>"))[-1]
    header <- gsub("\\<t[^\\>]*\\>","",header,perl=T)
    header <- gsub("\\</t[^\\>]*\\>","",header,perl=T)
    header <- gsub(" ","",header)
    raw <- gsub("\\<t[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</t[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\<a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</a[^\\>]*\\>","",raw,perl=T)
    val <- matrix(raw[-1],ncol=length(header),byrow=T)
    val[,-1] <- gsub(" ","",val[,-1])
    val[,1] <- gsub("      ","",val[,1])
    colnames(val) <- header
    return(val)    
  }

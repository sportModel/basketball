scrub <- function(raw)
  {
    raw <- raw[setdiff(1:length(raw),grep("table",raw))]
    raw <- raw[setdiff(1:length(raw),grep("thead",raw))]
    raw <- raw[setdiff(1:length(raw),grep("tbody",raw))]
    raw <- raw[setdiff(1:length(raw),grep("<colgroup>",raw,fixed=TRUE))]
    raw <- raw[setdiff(1:length(raw),grep("<tr",raw,fixed=TRUE))]
    raw <- raw[setdiff(1:length(raw),grep("</tr",raw,fixed=TRUE))]
    raw <- gsub("\\<th[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</th\\>","",raw,perl=T)
    raw <- gsub("\\<td[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</td\\>","",raw,perl=T)
    raw <- gsub("\\<a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</A\\>","",raw,perl=T)
    raw <- gsub("\\</a\\>","",raw,perl=T)
    raw <- gsub("\\<font[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</font\\>","",raw,perl=T)
    raw <- gsub("\\<span[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</span\\>","",raw,perl=T)
    raw <- gsub("\\<center[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</center\\>","",raw,perl=T)
    raw <- gsub("\\<b[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</b\\>","",raw,perl=T)
    raw <- gsub("&nbsp;","",raw,fixed=TRUE)
    raw <- gsub("%","",raw,fixed=TRUE)
    raw <- gsub("   ","",raw,perl=T)
    raw <- gsub("  ","",raw,perl=T)
    return(raw)
  }

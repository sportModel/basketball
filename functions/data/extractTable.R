extractTable <- function(raw,text)
  {
    tables <- sort(c(grep("<table",raw,fixed=T),grep("</table",raw,fixed=T)))
    target.table <- grep(text,raw[tables])[1]
    return(raw[tables[target.table]:tables[target.table+1]])
  }

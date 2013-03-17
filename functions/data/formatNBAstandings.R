formatNBAstandings <- function()
  {
    raw <- readLines(paste("data/nba/",par@year,"/raw/nba.html",sep=""),warn=F)
    raw <- extractTable(raw,"E_standings")
    raw <- scrub(raw)
    E <- matrix(raw,ncol=8,byrow=TRUE)
    raw <- readLines(paste("data/nba/",par@year,"/raw/nba.html",sep=""),warn=F)
    raw <- extractTable(raw,"W_standings")
    raw <- scrub(raw)
    W <- matrix(raw,ncol=8,byrow=TRUE)
    NBA <- rbind(E,W)
    NBA[,1] <- gsub("\\*\\(.*\\)","",NBA[,1])
    NBA[,1] <- gsub("\\(.*\\)","",NBA[,1])
    div.id <- which(NBA[,2]=="")
    div <- vector("list",length(div.id))
    for (i in 1:length(div.id))
      {
        n.t <- suppressWarnings(sum(!is.na(cumsum(as.numeric(NBA[-(1:div.id[i]),2])))))
        div[[i]] <- NBA[(div.id[i]+1):(div.id[i]+n.t),]
        rownames(div[[i]]) <- div[[i]][,1]
        div[[i]] <- div[[i]][,-1]
        colnames(div[[i]]) <- NBA[1,-1]
        div[[i]] <- as.data.frame(div[[i]])
        for (j in 1:ncol(div[[i]])) div[[i]][,j] <- suppressWarnings(as.numeric(div[[i]][,j]))
        div[[i]]$GB[is.na(div[[i]]$GB)] <- 0
        names(div)[i] <- NBA[div.id[i],1]
      }
    return(div)
  }

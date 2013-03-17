formatNCAAplayer <- function(team)
  {
    raw <- readLines(paste("data/ncaa/",par@year,"/raw/",team,"_player.html",sep=""),warn=F)
    tab <- NULL
    for (i in 1:5)
      {
        raw.i <- extractTable(raw,paste("statTab",i,sep=""))
        raw.i <- unlist(strsplit(paste(raw.i,collapse=""),split="<td>"))
        header <- unlist(strsplit(raw.i[1],split="<th>"))[-1]
        header <- gsub("\\<t[^\\>]*\\>","",header,perl=T)
        header <- gsub("\\</t[^\\>]*\\>","",header,perl=T)
        header <- gsub(" ","",header)
        raw.i <- gsub("\\<t[^\\>]*\\>","",raw.i,perl=T)
        raw.i <- gsub("\\</t[^\\>]*\\>","",raw.i,perl=T)
        raw.i <- gsub("\\<a[^\\>]*\\>","",raw.i,perl=T)
        raw.i <- gsub("\\</a[^\\>]*\\>","",raw.i,perl=T)
        tab[[i]] <- matrix(raw.i[-1],ncol=length(header),byrow=T)
        colnames(tab[[i]]) <- header
        name.col <- which(header=="Name")
        tab[[i]][,-name.col] <- gsub(" ","",tab[[i]][,-name.col])
        tab[[i]][,name.col] <- gsub("      ","",tab[[i]][,name.col])
        ##tab[[i]] <- cbind(formatName(tab[[i]][,name.col]),tab[[i]][,-name.col])
        tab[[i]] <- tab[[i]][order(tab[[i]][,name.col]),]
        if (i==1) val <- tab[[i]]
        else
          {
            ##ind <- match(apply(tab[[i]][,1:2],1,paste,collapse=" "),apply(val[,1:2],1,paste,collapse=" "))
            ind <- match(tab[[i]][,name.col],val[,"Name"])
            val <- cbind(val[ind,],tab[[i]][,-name.col])
          }
      }
    val <- data.frame(team,val[,c("Name","No.","Yr.")],Pos="",val[,c("Ht.","Wt.","Played","Started","Minutes","FGMade","FGAttempts","3ptFGMade","3ptFGAttempts","FTMade","FTAttempts","OffensiveTotal","DefensiveTotal","Assists","Steals","Blocks","Turnovers","Points")])
    colnames(val) <- c("Team","Name","No","Yr","Pos","Ht","Wt","G","GS","MP","FG","FGA","3P","3PA","FT","FTA","ORB","DRB","AST","STL","BLK","TOV","PTS")
    val$Ht[val$Ht==""] <- "6-5"
    val$Wt[val$Wt==0] <- 209
    for (i in 7:(dim(val)[2])) val[,i] <- as.numeric(val[,i])
    return(val)
  }

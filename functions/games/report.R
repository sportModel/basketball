report <- function(filename,level=NULL)
  {
    tree <- strsplit(filename,"/")[[1]]
    if (tree[2]=="ncaa")
      {
        level="ncaa"
        assign("par",setPar("ncaa",as.numeric(tree[3])),env=.GlobalEnv)
      }
    if (tree[2]=="nba")
      {
        level="nba"
        assign("par",setPar("nba",as.numeric(tree[3])),env=.GlobalEnv)
      }
    options(width=150)
    boxes <- formatGame(filename)
    team1.box <- boxes[[1]]
    team2.box <- boxes[[2]]
    game.poss <- estimatePoss(boxes)

    ff1 <- fourFactors(team1.box)
    ff2 <- fourFactors(team2.box)
    exp.diff <- expDiff(ff1,ff2,game.poss)

    miss1 <- sum(team1.box[,"FGA"]-team1.box[,"FG"])
    miss2 <- sum(team2.box[,"FGA"]-team2.box[,"FG"])
    tf1 <- calcTF(team1.box,TeamPoss=game.poss,TeamDMisses=miss2,game=T)
    tf2 <- calcTF(team2.box,TeamPoss=game.poss,TeamDMisses=miss1,game=T)
    rownames(tf1) <- tf1[,1]
    rownames(tf2) <- tf2[,1]

    vc1 <- calcVC(tf1,game=T)
    vc2 <- calcVC(tf2,game=T)
    vc1[,"WC"] <- 10*vc1[,"WC"]
    vc2[,"WC"] <- 10*vc2[,"WC"]

    ## Formatting for display
    display.categories.raw <- c("Pos","MP","FG","FGA","3P","3PA","FT","FTA","ORB","DRB","AST","TOV","STL","BLK","PTS")
    raw1 <- team1.box[,display.categories.raw]
    totals1 <- apply(raw1[,-1:-2],2,sum)
    raw1 <- rbind(raw1,c("","",totals1),c(rep("",2),round(100*totals1["FG"]/totals1["FGA"],digits=1),"",round(100*totals1["3P"]/totals1["3PA"],digits=1),"",round(100*totals1["FT"]/totals1["FTA"],digits=1),rep("",8)))
    raw2 <- team2.box[,display.categories.raw]
    totals2 <- apply(raw2[,-1:-2],2,sum)
    raw2 <- rbind(raw2,c("","",totals2),c(rep("",2),round(100*totals2["FG"]/totals2["FGA"],digits=1),"",round(100*totals2["3P"]/totals2["3PA"],digits=1),"",round(100*totals2["FT"]/totals2["FTA"],digits=1),rep("",8)))
    rownames(raw1) <- c(team1.box$Name,"TOTAL","PCT")
    rownames(raw2) <- c(team2.box$Name,"TOTAL","PCT")
    tf1 <- tf1[,c(-1:-8,-21:-30)]
    tf2 <- tf2[,c(-1:-8,-21:-30)]
    vc1=round(vc1[,-1:-8],digits=1)
    vc2=round(vc2[,-1:-8],digits=1)

    printSummary(boxes,game.poss)
    
    val <- list(raw1=raw1,
                raw2=raw2,
                ff1=ff1,
                ff2=ff2,
                exp.diff=exp.diff,
                tfs1=round(tf1,digits=1),
                tfs2=round(tf2,digits=1),
                vc1=vc1[order(vc1$WC,decreasing=TRUE),],
                vc2=vc2[order(vc2$WC,decreasing=TRUE),])
    val
  }

ncaag <- function(url, year=2017) {
  # Read in data
  boxes <- ncaagFormat(url)
  team1.box <- boxes[[1]]
  team2.box <- boxes[[2]]
  assign("par",setPar("ncaa", year), env=.GlobalEnv)

  game.poss <- estimatePoss(boxes)
  ff1 <- fourFactors(team1.box)
  ff2 <- fourFactors(team2.box)
  exp.diff <- expDiff(ff1,ff2,game.poss)

  miss1 <- sum(team1.box[,"FGA"]-team1.box[,"FG"])
  miss2 <- sum(team2.box[,"FGA"]-team2.box[,"FG"])
  tf1 <- calcTF(team1.box, TeamPoss=game.poss, TeamDMisses=miss2, game=T)
  tf2 <- calcTF(team2.box, TeamPoss=game.poss, TeamDMisses=miss1, game=T)
  rownames(tf1) <- tf1[,1]
  rownames(tf2) <- tf2[,1]

  vc1 <- calcVC(tf1, game=T)
  vc2 <- calcVC(tf2, game=T)
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
  vc1 <- round(vc1[,-1:-8],digits=1)
  vc2 <- round(vc2[,-1:-8],digits=1)
  ff1[1:5] <- 100*ff1[1:5]
  ff2[1:5] <- 100*ff2[1:5]
  vc1 <- vc1[,-ncol(vc1)]
  vc2 <- vc2[,-ncol(vc2)]
  vc1[,-1] <- round(vc1[,-1]/100*vc1$TotalPoss,1)
  vc2[,-1] <- round(vc2[,-1]/100*vc2$TotalPoss,1)

  printSummary(boxes,game.poss)

  val <- list(raw1=raw1,
              raw2=raw2,
              ff1 = round(ff1),
              ff2 = round(ff2),
              exp.diff=exp.diff,
              tfs1=round(tf1,digits=1),
              tfs2=round(tf2,digits=1),
              vc1=vc1[order(vc1$VC.Ovr,decreasing=TRUE),],
              vc2=vc2[order(vc2$VC.Ovr,decreasing=TRUE),])
  val
}

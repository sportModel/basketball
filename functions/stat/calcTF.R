calcTF <- function(raw, teamDT, team.opp=NULL, game=F, TeamPoss=NULL, TeamDMisses=NULL) {
  raw <- raw[raw$MP!=0,]
  team <- par@team
  model <- par@model
  min.game <- par@min.game
  
  ## Calculate necessary team-level statistics
  if (game) {
    TeamAssistRate <- sum(raw$AST) / sum(raw$FG)
    TeamOMisses <- sum(raw$FGA-raw$FG)
    raw$Team <- 1
    TeamG <- c(1,1)
    names(TeamG) <- 1:2
    Usg <- calcUsage(raw)
  }
  
  ## Calculate individual TFS
  M2 <- raw$`FG` - raw$`3P`
  A2 <- raw$`FGA` - raw$`3PA`
  Pct2 <- 100 * M2 / A2
  Pct1 <- 100 * raw$FT / raw$FTA
  Pct3 <- 100 * raw$`3P` / raw$`3PA`
  eFG <- 100 * (M2 + 1.5 * raw$`3P`) / (A2 + raw$`3PA`)
  TSP <- 100 * (M2 + 1.5 * raw$`3P` + 0.5 * raw$FT) / (A2 + raw$`3PA` + 0.5 * raw$FTA)
  
  w <- raw$MP / (min.game*teamDT[raw$Team, G])
  TotalPoss <- w * teamDT[raw$Team, Poss] * teamDT[raw$Team, G]
  OMisses <- w * teamDT[raw$Team, TmMiss]
  DMisses <- w * teamDT[raw$Team, OpMiss]
  ORebPct <- 100 * raw$ORB / OMisses
  DRebPct <- 100 * raw$DRB / DMisses
  
  Pts100 <- 100 * raw$PTS / TotalPoss
  TO100 <- 100 * raw$TOV / TotalPoss
  Ast100 <- 100 * raw$AST / TotalPoss
  M2100 <- 100 * M2 / TotalPoss
  M3100 <- 100 * raw$`3P` / TotalPoss
  Miss100 <- 100 * (raw$FGA - raw$FG) / TotalPoss
  Miss2100 <- 100 * (A2 - M2) / TotalPoss
  Miss3100 <- 100 * (raw$`3PA` - raw$`3P`) / TotalPoss
  M1100 <- 100 * raw$FT / TotalPoss
  Miss1100 <- 100 * (raw$FTA - raw$FT) / TotalPoss
  OMisses100 <- 100 * OMisses / TotalPoss
  DMisses100 <- 100 * DMisses / TotalPoss
  DReb100 <- 100 * raw$DRB / TotalPoss
  Stl100 <- 100 * raw$STL / TotalPoss
  Blk100 <- 100 * raw$BLK / TotalPoss
  
  ## Assisted-on data/estimation
  if (par@level=="nba") {
    Astd2 <- raw$Astd2
    Astd3 <- raw$Astd2
  } else {
    ##a <- Ast100 - by(Ast100, team, mean)
    ##s <- 100*raw$FGA/TotalPoss
    Astd2 <- Astd3 <- teamDT[raw$Team, TmAstRate]
  }

  if (!game) {
    if (par@level=='nba') {
      
      X <- raw[,c(1:7)]
    } else {
      X <- raw[,c(1:2, 7:11)]
    }
    val <- data.table(X, TotalPoss,Pts100,Pct1,Pct2,Pct3,eFG,TSP,Ast100,TO100,ORebPct,DRebPct,Stl100,Blk100,M1100,M2100,M3100,Miss1100,Miss2100,Miss3100,OMisses100,DReb100,Astd2, Astd3)
  } else {
    val <- cbind(raw[,1:8],TotalPoss,Usg,Pts100,Pct1,Pct2,Pct3,eFG,TSP,Ast100,TO100,ORebPct,DRebPct,Stl100,Blk100,M1100,M2100,M3100,Miss1100,Miss2100,Miss3100,OMisses100,DReb100,AssistRate)
  }
  val
}

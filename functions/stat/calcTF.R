calcTF <- function(raw, teamDT=NULL, team.opp=NULL, game=FALSE, TeamPoss=NULL, TeamDMisses=NULL) {
  raw <- raw[raw$MP!=0,]
  team <- par@team
  model <- par@model
  min.game <- par@min.game

  ## Calculate necessary team-level statistics
  if (game) {
    teamDT <- data.table(Team = raw$Team[1],
                         G = 1,
                         Poss = TeamPoss,
                         TmMiss = sum(raw$FGA-raw$FG),
                         OpMiss = TeamDMisses,
                         TmAstRate = sum(raw$AST) / sum(raw$FG))
    setkey(teamDT, Team)
    Usg <- calcUsage(raw)
  }

  ## Calculate individual TFS
  M2 <- raw$`2P`
  A2 <- raw$`2PA`
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
  if (par@level=="nba" & !game) {
    Astd2 <- raw$Astd2
    Astd3 <- raw$Astd3
  } else {
    ##a <- Ast100 - by(Ast100, team, mean)
    ##s <- 100*raw$FGA/TotalPoss
    Astd2 <- Astd3 <- teamDT[raw$Team, TmAstRate]
  }

  if (!game) {
    if (par@level=='nba') {
      X <- raw[,c(1:7)]
    } else {
      X <- raw[,c(1:2, 4:8)]
    }
    val <- data.table(X, TotalPoss,Pts100,Pct1,Pct2,Pct3,eFG,TSP,Ast100,TO100,ORebPct,DRebPct,Stl100,Blk100,M1100,M2100,M3100,Miss1100,Miss2100,Miss3100,OMisses100,DReb100,Astd2, Astd3)
  } else {
    val <- cbind(raw[,1:7],TotalPoss,Usg,Pts100,Pct1,Pct2,Pct3,eFG,TSP,Ast100,TO100,ORebPct,DRebPct,Stl100,Blk100,M1100,M2100,M3100,Miss1100,Miss2100,Miss3100,OMisses100,DReb100, Astd2, Astd3)
  }
  val
}

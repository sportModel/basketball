## NOTE: NEED TO FIX TeamG for NCAA
calcTF <- function(raw,TeamG=1,team.opp=NULL,game=F,TeamPoss=NULL,TeamDMisses=NULL)
  {
    raw <- raw[raw[,"MP"]!=0,]
    team <- par@team
    model <- par@model
    min.game <- par@min.game

    ## Calculate necessary team-level statistics
    if (!game)
      {
        TeamPoss <- TeamAssistRate <- TeamOMisses <- numeric(length(team))
        names(TeamPoss) <- names(TeamAssistRate) <- names(TeamOMisses) <- team
        for (i in team)
          {
            team.i <- raw[raw[,"Team"]==i,]
            TeamPoss[i] <- sum(team.i[,"FGA"]+par@constants[2]*team.i[,"FTA"]+team.i[,"TOV"]-team.i[,"ORB"])
            TeamAssistRate[i] <- sum(team.i[,"AST"]) / sum(team.i[,"FG"])
            TeamOMisses[i] <- sum(team.i[,"FGA"]-team.i[,"FG"])
          }
        TeamDMisses <- TeamPoss * model$eM100 / 100
        names(TeamG) <- team
      }
    else
      {
        TeamAssistRate <- sum(raw[,"AST"]) / sum(raw[,"FG"])
        TeamOMisses <- sum(raw[,"FGA"]-raw[,"FG"])
        raw[,"Team"] <- 1
        TeamG <- c(1,1)
        names(TeamG) <- 1:2
      }
    
    ## Calculate individual TFS
    M2 <- raw[,"FG"] - raw[,"3P"]
    A2 <- raw[,"FGA"] - raw[,"3PA"]
    Pct2 <- 100 * M2 / A2
    Pct1 <- 100 * raw[,"FT"] / raw[,"FTA"]
    Pct3 <- 100 * raw[,"3P"] / raw[,"3PA"]
    eFG <- 100 * (M2 + 1.5 * raw[,"3P"]) / (A2 + raw[,"3PA"])
    TSP <- 100 * (M2 + 1.5 * raw[,"3P"] + 0.5 * raw[,"FT"]) / (A2 + raw[,"3PA"] + 0.5 * raw[,"FTA"])

    w <- raw[,"MP"] / (min.game*TeamG[raw[,"Team"]])
    TotalPoss <- TeamPoss[raw[,"Team"]] * w
    OMisses <- TeamOMisses[raw[,"Team"]] * w
    DMisses <- TeamDMisses[raw[,"Team"]] * w
    ORebPct <- 100 * raw[,"ORB"] / OMisses
    DRebPct <- 100 * raw[,"DRB"] / DMisses
    if (par@level=="nba") {
      AssistRate <- raw[,"Astd"]
    } else AssistRate <- TeamAssistRate[raw[,"Team"]]
    AssistRate[!is.finite(AssistRate)] <- TeamAssistRate[raw[,"Team"]][!is.finite(AssistRate)]
    
    Pts100 <- 100 * raw[,"PTS"] / TotalPoss
    TO100 <- 100 * raw[,"TOV"] / TotalPoss
    Ast100 <- 100 * raw[,"AST"] / TotalPoss
    M2100 <- 100 * M2 / TotalPoss
    M3100 <- 100 * raw[,"3P"] / TotalPoss
    Miss100 <- 100 * (raw[,"FGA"] - raw[,"FG"]) / TotalPoss
    Miss2100 <- 100 * (A2 - M2) / TotalPoss
    Miss3100 <- 100 * (raw[,"3PA"] - raw[,"3P"]) / TotalPoss
    M1100 <- 100 * raw[,"FT"] / TotalPoss
    Miss1100 <- 100 * (raw[,"FTA"] - raw[,"FT"]) / TotalPoss
    OMisses100 <- 100 * OMisses / TotalPoss
    DMisses100 <- 100 * DMisses / TotalPoss
    DReb100 <- 100 * raw[,"DRB"] / TotalPoss
    Stl100 <- 100 * raw[,"STL"] / TotalPoss
    Blk100 <- 100 * raw[,"BLK"] / TotalPoss
    val <- cbind(raw[,1:8],TotalPoss,Pts100,Pct1,Pct2,Pct3,eFG,TSP,Ast100,TO100,ORebPct,DRebPct,Stl100,Blk100,M1100,M2100,M3100,Miss1100,Miss2100,Miss3100,OMisses100,DReb100,AssistRate)
    val
  }

fourFactors <- function(team)
  {
    FGA <- sum(team[,"FGA"])
    M2 <- team[,"FG"]-team[,"3P"]
    FG <- sum(team[,"FG"])
    eFG <- sum(M2+1.5*team[,"3P"])/FGA
    OReb <- sum(team[,"ORB"])/(FGA-FG)
    TOV <- sum(team[,"TOV"])
    FTV <- sum(team[,"FT"]) - 0.5*sum(team[,"FTA"])

    val <- c(eFG,OReb,TOV,FTV)
    names(val) <- c("eFG%","OReb%","TOV","FTV")
    val
  }

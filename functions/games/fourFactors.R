fourFactors <- function(team) {
  FGA <- sum(team[,"FGA"])
  M2 <- team[,"FG"]-team[,"3P"]
  FG <- sum(team[,"FG"])
  eFG <- sum(M2+1.5*team[,"3P"])/FGA
  OReb <- sum(team[,"ORB"])/(FGA-FG)
  TOV <- sum(team[,"TOV"])
  FTV <- sum(team[,"FT"]) - 0.5*sum(team[,"FTA"])
  M3 <- sum(team$'3P')
  A3 <- sum(team$'3PA')
  Pct2 <- sum(M2)/(FGA-A3)
  Pct3 <- M3/A3
  Frac <- A3/FGA

  val <- c(Pct2, Pct3, Frac, eFG, OReb, TOV, FTV)
  names(val) <- c("2%", "3%", "3/FGA", "eFG%", "OReb%", "TOV", "FTV")
  val
}

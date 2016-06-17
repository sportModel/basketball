estimatePoss <- function(boxes) {
  A <- boxes[[1]][,"FGA"]
  team1.pos <- sum(A-boxes[[1]][,"ORB"]+boxes[[1]][,"TOV"]+.475*boxes[[1]][,"FTA"])
  A <- boxes[[2]][,"FGA"]
  team2.pos <- sum(A-boxes[[2]][,"ORB"]+boxes[[2]][,"TOV"]+.475*boxes[[2]][,"FTA"])
  return((team1.pos+team2.pos)/2)
}

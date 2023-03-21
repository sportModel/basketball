calcWVC <- function(X, team.raw) {
  ind <- which(substr(names(X), 1, 2)=="VC")
  TmAdj <- par@poss/team.raw[X$Team,"Poss"]
  X[,ind] <- (X[,ind] / 100) * (X$TotalPoss / team.raw[X$Team,"TmG"]) * TmAdj
  X
}
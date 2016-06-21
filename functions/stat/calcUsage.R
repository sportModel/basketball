# Formula from http://www.basketball-reference.com/about/glossary.html#usg_pct
calcUsage <- function(raw) {
  w <- with(raw, FGA + 0.44 * FTA + TOV)
  mp <- raw$MP
  100 * (w * sum(mp)/5) / (mp*sum(w))
}

## Unfinished!
calcUsageExperimental <- function(raw) {
  for (team.i in par@team) {
    raw.i <- subset(raw, Team==team.i)
    A1 <- 0.5*raw.i[,"FTA"]
    A3 <- raw.i[,"3PA"]
    A2 <- raw.i$FGA - A3
    TO <- raw.i[,"TOV"]
    X1 <- cbind(A1, A2, A3, TO)
    w <- raw.i$MP/(sum(raw.i$MP)/5)
    E1 <- cbind(X1, apply(X1, 1, sum))/sum(X1)
    E2 <- E1/w
    E3 <- sweep(X1, 2, apply(X1, 2, sum), "/")
    E4 <- E3/w
    E <- cbind(E1, E2, E3, E4)
    rownames(E) <- raw.i$Name
    E <- E[order(E[,5], decreasing=TRUE),]
    round(100*E,1)
  }
}
expDiff <- function(ff1, ff2, poss) {
  Diff <- numeric(5)
  names(Diff) <- c("FG", "RB", "TO", "FT", "Tot")
  Diff['FG'] <- poss*(ff2['eFG%']-ff1['eFG%'])
  Diff['RB'] <- poss*(ff2['OReb%']-ff1['OReb%'])/2
  Diff['TO'] <- ff1['TOV'] - ff2['TOV']
  Diff['FT'] <- ff2['FTV'] - ff1['FTV']
  Diff['Tot'] <- sum(Diff)
  round(Diff,1)
}

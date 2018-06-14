makeTeamComparison <- function(team) {
  ## Team
  TmM2 <- with(team, TmFG - Tm3P)
  TmeFG <- with(team, (TmM2 + 1.5*Tm3P)/TmFGA)
  TmOReb <- with(team, TmORB/(TmFGA-TmFG))
  Poss <- team$Poss
  TmTOV <- with(team, 100*TmTOV/Poss)
  TmFTV <- with(team, 100*(TmFT-0.5*TmFTA)/Poss)

  ## Opponent
  OpM2 <- with(team, OpFG - Op3P)
  OpeFG <- with(team, (OpM2 + 1.5*Op3P)/OpFGA)
  OpOReb <- with(team, OpORB/(OpFGA-OpFG))
  OpTOV <- with(team, 100*OpTOV/Poss)
  OpFTV <- with(team, 100*(OpFT-0.5*OpFTA)/Poss)

  # Ranks
  if (par@level=='nba') {
    displayNames <- team$FullName
  } else {
    displayNames <- conf[team$Team, "Display"]
  }
  X <- data.frame(TmeFG, TmOReb, TmTOV, TmFTV, OpeFG, OpOReb, OpTOV, OpFTV, row.names=)
  X <- X[order(X$TmeFG, decreasing=TRUE),]
  R <- X
  R[,1] <- rank(-R[,1])
  R[,2] <- rank(-R[,2])
  R[,3] <- rank(R[,3])
  R[,4] <- rank(-R[,4])
  R[,5] <- rank(R[,5])
  R[,6] <- rank(R[,6])
  R[,7] <- rank(-R[,7])
  R[,8] <- rank(R[,8])

  # Values (Expected point diff)
  XX <- scale(X, scale=FALSE)
  V <- X
  AvgPoss <- mean(team$Poss)
  V[,1] <- AvgPoss*(XX[,1])
  V[,2] <- AvgPoss*(XX[,2])/2
  V[,3] <- AvgPoss/100*(-XX[,3])
  V[,4] <- AvgPoss/100*(XX[,4])
  V[,5] <- AvgPoss*(-XX[,5])
  V[,6] <- AvgPoss*(-XX[,6])/2
  V[,7] <- AvgPoss/100*(XX[,7])
  V[,8] <- AvgPoss/100*(-XX[,8])
  V$Total <- apply(V, 1, sum)

  ## Output
  filename <- paste(par@loc, "/", par@level, "/", par@year, "/team_comparison.html", sep="")
  Xh <- htmlTable(X, class="'sortable ctable'", digits=rep(c(2,2,0,1),2))
  Rh <- htmlTable(R, class="'sortable ctable'", digits=0)
  Vh <- htmlTable(V, class="'sortable ctable'", digits=1)
  L <- c(Xh, Rh, Vh)
  cat(paste0("---\nlevel: ", par@level, "\nyear: ", par@year, "\nrel: ../../\n---\n"), file=filename)
  print(htmlList(L), file=filename, append=TRUE)
}

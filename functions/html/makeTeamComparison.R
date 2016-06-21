makeTeamComparison <- function(team.raw) {
  ## Team
  TmM2 <- with(team.raw, TmFG - Tm3P)
  TmeFG <- with(team.raw, (TmM2 + 1.5*Tm3P)/TmFGA)
  TmOReb <- with(team.raw, TmORB/(TmFGA-TmFG))
  TmPoss <- with(team.raw, TmFGA+par@constants[2]*TmFTA+TmTOV-TmORB)
  TmTOV <- with(team.raw, 100*TmTOV/TmPoss)
  TmFTV <- with(team.raw, 100*(TmFT-0.5*TmFTA)/TmPoss)

  ## Opponent
  OpM2 <- with(team.raw, OpFG - Op3P)
  OpeFG <- with(team.raw, (OpM2 + 1.5*Op3P)/OpFGA)
  OpOReb <- with(team.raw, OpORB/(OpFGA-OpFG))
  OpPoss <- with(team.raw, OpFGA+par@constants[2]*OpFTA+OpTOV-OpORB)
  OpTOV <- with(team.raw, 100*OpTOV/OpPoss)
  OpFTV <- with(team.raw, 100*(OpFT-0.5*OpFTA)/OpPoss)

  # Ranks
  X <- data.frame(TmeFG, TmOReb, TmTOV, TmFTV, OpeFG, OpOReb, OpTOV, OpFTV, row.names=conf[rownames(team.raw), "Display"])
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
  AvgPoss <- mean(team.raw[,"Poss"])
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
  filename <- paste(par@loc,"/",par@level,"_",par@year,"_team_comparison.html",sep="")
  Xh <- htmlTable(X, class="'sortable ctable'", digits=rep(c(2,2,0,1),2))
  Rh <- htmlTable(R, class="'sortable ctable'", digits=0)
  Vh <- htmlTable(V, class="'sortable ctable'", digits=1)
  L <- c(Xh, Rh, Vh)
  print(htmlList(L), file=filename)
}

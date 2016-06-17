calcTeam <- function(team.raw) {
  ## Possessions and Plays
  TmORBPct <- team.raw[,"TmORB"]/(team.raw[,"TmORB"]+team.raw[,"OpDRB"])
  OpORBPct <- team.raw[,"OpORB"]/(team.raw[,"OpORB"]+team.raw[,"TmDRB"])
  TmPoss <- team.raw[,"TmFGA"]-TmORBPct*(team.raw[,"TmFGA"]-team.raw[,"TmFG"])*par@constants[1]+team.raw[,"TmTOV"]+team.raw[,"TmFTA"]*par@constants[2]
  OpPoss <- team.raw[,"OpFGA"]-OpORBPct*(team.raw[,"OpFGA"]-team.raw[,"OpFG"])*par@constants[1]+team.raw[,"OpTOV"]+team.raw[,"OpFTA"]*par@constants[2]
  Poss <- (TmPoss+OpPoss)/2
  TmPlays <- team.raw[,"TmFGA"]+team.raw[,"TmTOV"]+team.raw[,"TmFTA"]*par@constants[2]
  OpPlays <- team.raw[,"OpFGA"]+team.raw[,"OpTOV"]+team.raw[,"OpFTA"]*par@constants[2]
  
  ## Summary stats
  TmEff <- team.raw[,"TmPTS"]/TmPoss
  Tm2P <- team.raw[,"TmFG"]-team.raw[,"Tm3P"]
  Tm2PA <- team.raw[,"TmFGA"]-team.raw[,"Tm3PA"]
  Tm2PPct <- Tm2P/Tm2PA
  Tm3PPct <- team.raw[,"Tm3P"]/team.raw[,"Tm3PA"]
  TmFTPct <- team.raw[,"TmFT"]/team.raw[,"TmFTA"]
  Tm3PR <- team.raw[,"Tm3PA"]/team.raw[,"TmFGA"]
  TmPPG <- team.raw[,"TmPTS"]/team.raw[,"TmG"]
  OpEff <- team.raw[,"OpPTS"]/OpPoss
  Op2P <- team.raw[,"OpFG"]-team.raw[,"Op3P"]
  Op2PA <- team.raw[,"OpFGA"]-team.raw[,"Op3PA"]
  Op2PPct <- Op2P/Op2PA
  Op3PPct <- team.raw[,"Op3P"]/team.raw[,"Op3PA"]
  OpFTPct <- team.raw[,"OpFT"]/team.raw[,"OpFTA"]
  Op3PR <- team.raw[,"Op3PA"]/team.raw[,"OpFGA"]
  OpPPG <- team.raw[,"OpPTS"]/team.raw[,"OpG"]
  
  ## PrWin
  TmAlpha <- ((1-(3*team.raw[,"TmFGA"]/TmPlays*Tm3PR*Tm3PPct+2*team.raw[,"TmFGA"]/TmPlays*(1-Tm3PR)*Tm2PPct+team.raw[,"TmFTA"]/TmPlays*TmFTPct)/TmEff)/TmORBPct - (team.raw[,"TmFGA"]-team.raw[,"TmFG"])/TmPlays)*TmPlays/(team.raw[,"TmFTA"]-team.raw[,"TmFT"])
  E.Y1 <- (3*team.raw[,"TmFGA"]/TmPlays*Tm3PR*Tm3PPct+2*team.raw[,"TmFGA"]/TmPlays*(1-Tm3PR)*Tm2PPct+team.raw[,"TmFTA"]/TmPlays*TmFTPct)/(1-((team.raw[,"TmFGA"]-team.raw[,"TmFG"])/TmPlays + (team.raw[,"TmFTA"]-team.raw[,"TmFT"])/TmPlays*TmAlpha)*TmORBPct)
  E.Y12 <- (9*team.raw[,"TmFGA"]/TmPlays*Tm3PR*Tm3PPct+4*team.raw[,"TmFGA"]/TmPlays*(1-Tm3PR)*Tm2PPct+par@constants[2]*team.raw[,"TmFTA"]/TmPlays*(TmFTPct/par@constants[2])^2)/(1-((team.raw[,"TmFGA"]-team.raw[,"TmFG"])/TmPlays + (team.raw[,"TmFTA"]-team.raw[,"TmFT"])/TmPlays*TmAlpha)*TmORBPct)
  E.X1 <- Poss/team.raw[,"TmG"]*E.Y1
  var.X1 <- Poss/team.raw[,"TmG"]*(E.Y12-E.Y1^2)
  OpAlpha <- ((1-(3*team.raw[,"OpFGA"]/OpPlays*Op3PR*Op3PPct+2*team.raw[,"OpFGA"]/OpPlays*(1-Op3PR)*Op2PPct+team.raw[,"OpFTA"]/OpPlays*OpFTPct)/OpEff)/OpORBPct - (team.raw[,"OpFGA"]-team.raw[,"OpFG"])/OpPlays)*OpPlays/(team.raw[,"OpFTA"]-team.raw[,"OpFT"])
  E.Y2 <- (3*team.raw[,"OpFGA"]/OpPlays*Op3PR*Op3PPct+2*team.raw[,"OpFGA"]/OpPlays*(1-Op3PR)*Op2PPct+team.raw[,"OpFTA"]/OpPlays*OpFTPct)/(1-((team.raw[,"OpFGA"]-team.raw[,"OpFG"])/OpPlays + (team.raw[,"OpFTA"]-team.raw[,"OpFT"])/OpPlays*OpAlpha)*OpORBPct)
  E.Y22 <- (9*team.raw[,"OpFGA"]/OpPlays*Op3PR*Op3PPct+4*team.raw[,"OpFGA"]/OpPlays*(1-Op3PR)*Op2PPct+par@constants[2]*team.raw[,"OpFTA"]/OpPlays*(OpFTPct/par@constants[2])^2)/(1-((team.raw[,"OpFGA"]-team.raw[,"OpFG"])/OpPlays + (team.raw[,"OpFTA"]-team.raw[,"OpFT"])/OpPlays*OpAlpha)*OpORBPct)
  E.X2 <- Poss/team.raw[,"OpG"]*E.Y2
  var.X2 <- Poss/team.raw[,"OpG"]*(E.Y22-E.Y2^2)
  PrWin <- pnorm(0,E.X1-E.X2,sqrt(var.X1+var.X2+par@constants[3]),low=F)
  EW <- team.raw[,"TmG"]*PrWin
  EL <- team.raw[,"TmG"]*(1-PrWin)
  a <- 10
  PythWin <- TmPPG^a/(TmPPG^a+OpPPG^a)
  Pct <- team.raw[,"TmW"]/team.raw[,"TmG"]
  return(data.frame(TmPoss,EW,EL))
}

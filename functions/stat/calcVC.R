calcVC <- function(tf, teamDT, game=FALSE) {
  year <- par@year
  model <- par@model
  
  if (game) {
    TmPoss <- 1 ## Cancels out; irrelevant for game=TRUE
  } else {
    TmPoss <- teamDT$Poss
    names(TmPoss) <- teamDT$Team
  }
  
  vc <- matrix(0,nrow=nrow(tf),ncol=9)
  if (game) load(file=paste("data",par@level,year,"prototype.RData",sep="/"))
  else prototype <- matrix(NA,ncol=9,nrow=5)
  for (i in 1:5) {
    pos <- names(tf)[i+2]
    w1 <- tf[[pos]]
    w2 <- tf$TotalPoss
    norm <- sum(w1*w2)
    w2 <- w2/norm
    w <- w1*w2
    n <- length(w)
    
    VC.TO <- tf$TO100 * (-model$eP)
    VC.Ast <- tf$Ast100 * 0.5 * (2 - model$eP)
    VC.1 <- tf$M1100 * (1 - 0.5 * model$eP) + tf$Miss1100 * 0.5 * model$eP * (model$eR.FT - 1)
    VC.2 <- tf$M2100 * (2 - model$eP) * (1 - 0.5 * tf$Astd2) + tf$Miss2100 * model$eP * (model$eR - 1)
    VC.3 <- tf$M3100 * ((3 - model$eP) - (0.5 * tf$Astd3) * (2 - model$eP)) + tf$Miss3100 * model$eP * (model$eR - 1)
    VC.OReb <- tf$OMisses100 * model$eP * (tf$ORebPct / 100 - model$eR / 5)
    VC.DReb <- model$weight * model$dRebValue * (tf$DReb100 - model$eADReb100 / 5)
    VC.Stl <- model$weight * model$dStl100 * (tf$Stl100 - model$eStl100 / 5)
    VC.Blk <- model$weight * model$dBlk100 * (tf$Blk100 - model$eBlk100 / 5)
    
    vc.i <- cbind(VC.TO,VC.Ast,VC.1,VC.2,VC.3,VC.OReb,VC.DReb,VC.Stl,VC.Blk)
    if (!game) prototype[i,] <- apply(w*vc.i,2,sum)
    vc.i <- w1*(vc.i-outer(rep(1,n),prototype[i,]))
    ##if (i==1) vc1 <- vc.i
    ##if (i==2) vc2 <- vc.i
    ##if (i==3) vc3 <- vc.i
    ##if (i==4) vc4 <- vc.i
    ##if (i==5) vc5 <- vc.i
    vc <- vc + vc.i
  }
  vc <- cbind(tf[,1:9],vc)
  
  VC.Pass <- vc$VC.Ast + vc$VC.TO
  VC.Sc <- vc$VC.1 + vc$VC.2 + vc$VC.3
  VC.Reb <- vc$VC.OReb + vc$VC.DReb
  VC.BkSt <- vc$VC.Stl + vc$VC.Blk
  VC.Off <- vc$VC.Ast + vc$VC.TO + vc$VC.1 + vc$VC.2 + vc$VC.3 + vc$VC.OReb
  VC.Def <- vc$VC.DReb + vc$VC.Stl + vc$VC.Blk
  VC.Ovr <- VC.Off + VC.Def
  WC <- (vc$TotalPoss * mean(TmPoss) / TmPoss[vc$Team]) * VC.Ovr * model$p100.value / 100
  vc <- cbind(vc,VC.Pass,VC.Sc,VC.Reb,VC.BkSt,VC.Off,VC.Def,VC.Ovr,WC)
  
  if (!game) {
    dimnames(prototype) <- list(names(tf)[4:8],c("VC.TO","VC.Ast","VC.1","VC.2","VC.3","VC.OReb","VC.DReb","VC.Stl","VC.Blk"))        
    save(prototype,file=paste("data",par@level,year,"prototype.RData",sep="/"))
  }
  return(vc)
}

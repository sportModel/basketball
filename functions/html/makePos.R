makePos <- function(vc, team.raw) {
  posfile <- paste(par@loc,"/",par@level,"_",par@year,"_pos.html",sep="")
  cat("---\n---\n", file=posfile)
  for (Pos in c("pg","sg","sf","pf","c")) {
    cat("<a href=\"", par@level, "_", par@year, "_", Pos, ".html\">", toupper(Pos), "</a><br><br>\n", sep="", file=posfile, append=TRUE)
    X <- vc[vc$Pos==Pos,]
    rn <- X$Name
    repeat {
      ind <- duplicated(rn)
      if (sum(ind)==0) break
      else rn[ind] <- paste(rn[ind],"@@@@")
    }
    rownames(X) <- rn
    display.categories <- c("Team","Pos","VC.Pass","VC.Sc","VC.Reb","VC.BkSt","VC.Off","VC.Def","VC.Ovr","WC")
    display.digits <- c(0,0,0,rep(1,length(display.categories)-2))
    X <- X[X[,"TotalPoss"] >= 100,]
    ind <- which(substr(names(X), 1, 2)=="VC")
    TmAdj <- par@poss/team.raw[X$Team,"Poss"]
    X[,ind] <- (X[,ind] / 100) * (X$TotalPoss / team.raw[X$Team,"TmG"]) * TmAdj
    
    ind <- sort(X[,"WC"],ind=T,dec=T)$ix
    
    display <- xtable(X[ind,display.categories],digits=display.digits)
    align(display) <- rep("r",length(align(display)))
    align(display)[1:2] <- "l"
    
    filename <- paste(par@loc,"/",par@level,"_",par@year,"_",Pos,".html",sep="")
    print(htmlTable(display, class="'sortable ctable'"), file=filename)
    #cleanTable(filename)
  }
}

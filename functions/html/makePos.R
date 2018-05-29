makePos <- function(vc, team) {
  posfile <- paste(par@loc,"/",par@level,"_",par@year,"_pos.html",sep="")
  cat("---\n---\n", file=posfile)
  AllPos <- c("pg","sg","sf","pf","c")
  P <- as.matrix(vc[, AllPos, with=FALSE])
  majorityPos <- AllPos[apply(P, 1, which.max)]
  for (Pos in AllPos) {
    cat("<a href=\"", par@level, "_", par@year, "_", Pos, ".html\">", toupper(Pos), "</a><br><br>\n", sep="", file=posfile, append=TRUE)
    XT <- vc[Pos==majorityPos, .(Name, Team)][, .(Team=paste(Team, collapse='/')), Name]
    XXVC <- vc[Pos==majorityPos, c("Name", "TotalPoss", grep('VC', names(vc), value=TRUE)), with=FALSE]
    XVC <- XXVC[, lapply(.SD, weighted.mean, w=TotalPoss), Name]
    XWC <- vc[Pos==majorityPos, .(Name, WC)][, .(WC = sum(WC)), Name]
    setkey(XT, Name)
    setkey(XVC, Name)
    setkey(XWC, Name)
    X <- merge(XT, merge(XVC, XWC))[TotalPoss >= 100]
    display.categories <- c("Team","VC.Pass","VC.Sc","VC.Reb","VC.BkSt","VC.Off","VC.Def","VC.Ovr","WC")
    display.digits <- c(0,0,0,rep(1,length(display.categories)-2))

    display <- xtable(X[order(-WC), display.categories, with=FALSE],digits=display.digits)
    align(display) <- rep("r",length(align(display)))
    align(display)[1:2] <- "l"
    
    filename <- paste(par@loc,"/",par@level,"_",par@year,"_",Pos,".html",sep="")
    print(htmlTable(display, class="'sortable ctable'"), file=filename)
  }
}

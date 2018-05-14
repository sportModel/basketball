makeTeams.nba <- function(raw) {
  ## create list of matrices
  n.d <- length(raw$Standings)
  X <- D <- vector("list",n.d)
  for (d in 1:n.d) {
    X[[d]] <- raw$Standings[[d]]
    short.name <- raw$Team.Opp$Team[match(rownames(raw$Standings[[d]]),raw$Team.Opp$FullName)]
    long.name <- rownames(X[[d]])
    
    ## Format for printing
    link <- character(nrow(X[[d]]))
    for (i in 1:nrow(X[[d]])) {
      link[i] <- paste("@@lt@@A href=@@quote@@",par@level,"_",par@year,"_",short.name[i],".html@@quote@@ @@gt@@ ",long.name[i],"@@lt@@/a@@gt@@",sep="")
    }
    X[[d]] <- cbind(link,X[[d]][,1:6])
    colnames(X[[d]]) <- c("Team","W","L","Pct","GB","PS/G","PA/G")
    D[[d]] <- xtable(X[[d]])
    align(D[[d]]) <- c("l","l",rep("r",6))
    digits(D[[d]]) <- c(0,0,0,0,3,0,1,1)
  }
  
  ## display
  filename <- paste(par@loc,"/",par@level,"_",par@year,"_teams.html",sep="")
  cat("---\n---\n<TABLE class=\"container\">\n", file=filename)
  for (i in 1:3) {
    cat("<TR><TD align=\"center\">",names(raw$Standings)[i],"</TD><TD align=\"center\">",names(raw$Standings)[i+3],"</TD></TR>\n<TR><TD>",file=filename,append=TRUE)
    print(D[[i]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE)
    cat("</TD>\n<TD>",file=filename,append=TRUE)
    print(D[[i+3]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE)
    cat("</TD></TR>\n",file=filename,append=TRUE)
  }
  cat("</TABLE>\n",file=filename,append=TRUE)
  cleanTable(filename)
}

makeTeams.ncaa <- function(raw, team.raw) {
  ## create list of matrices
  div <- levels(relevel(as.factor(conf$Conf), "Other"))
  n.d <- length(div)
  X <- D <- vector("list", n.d)
  team.tf <- calcTeam(team.raw)
  PossG <- team.tf$TmPoss/team.raw$TmG
  PSP <- team.raw$TmPTS/team.tf$TmPoss
  PAG <- team.raw$OpPTS/team.tf$TmPoss
  Pct <- team.raw$TmW/(team.raw$TmG)
  names(PossG) <- names(PSP) <- names(PAG) <- names(Pct) <- rownames(team.raw)
  for (d in 1:n.d) {
    tm <- rownames(subset(conf, Conf==div[d] & rownames(conf) %in% par@team))
    X[[d]] <- data.frame(team.raw[tm, "TmW"], team.raw[tm, "TmL"], Pct[tm], PossG[tm], PSP[tm], PAG[tm])
    
    ## Format for printing
    link <- character(nrow(X[[d]]))
    for (i in 1:nrow(X[[d]])) {
      link[i] <- paste("@@lt@@A href=@@quote@@",par@level,"_",par@year,"_",tm[i],".html@@quote@@ @@gt@@ ",conf[tm[i],"Display"],"@@lt@@/a@@gt@@",sep="")
    }
    X[[d]] <- cbind(link,X[[d]])
    colnames(X[[d]]) <- c("Team","W","L","Pct","Poss/G", "PS/P","PA/P")
    X[[d]] <- X[[d]][order(X[[d]]$Pct, decreasing=TRUE),]
    D[[d]] <- xtable(X[[d]])
    align(D[[d]]) <- c("l","l",rep("r",6))
    digits(D[[d]]) <- c(0,0,0,0,3,0,2,2)
  }
  
  ## display
  filename <- paste(par@loc,"/",par@level,"_",par@year,"_teams.html",sep="")
  cat("---\n---\n<TABLE class=\"container\">\n", file=filename)
  for (r in 1:4) {
    i <- r + 1
    j <- if (r==3) 1 else r+5
    cat("<TR><TD align=\"center\">", div[i],"</TD>", file=filename, append=TRUE)
    if (j <= length(div)) {
      cat("<TD align=\"center\">", div[j], "</TD>", file=filename, append=TRUE)
    }
    cat("</TR>\n<TR><TD>", file=filename, append=TRUE)
    print(D[[i]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE)
    cat("</TD>\n<TD>",file=filename,append=TRUE)
    if (j <= length(D)) {
      print(D[[j]], type="html", include.rownames=FALSE, html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE)
    }    
    cat("</TD></TR>\n",file=filename,append=TRUE)
  }
  cat("</TABLE>\n",file=filename,append=TRUE)
  cleanTable(filename)
}

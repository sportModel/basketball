makeTeams.nba <- function(team, standings) {
  ## create list of matrices
  n.d <- length(standings)
  X <- D <- vector("list",n.d)
  for (d in 1:n.d) {
    X[[d]] <- standings[[d]]
    long.name <- stringr::str_trim(gsub('*', '', rownames(standings[[d]]), fixed=TRUE))
    short.name <- team$Team[match(long.name,team$FullName)]

    ## Format for printing
    link <- character(nrow(X[[d]]))
    for (i in 1:nrow(X[[d]])) {
      link[i] <- paste0('<a href="', short.name[i], '.html">', long.name[i] , '</a>')
    }
    X[[d]] <- cbind(link,X[[d]][,1:6])
    colnames(X[[d]]) <- c("Team","W","L","Pct","GB","PS/G","PA/G")
    D[[d]] <- xtable(X[[d]])
    align(D[[d]]) <- c("l","l",rep("r",6))
    digits(D[[d]]) <- c(0,0,0,0,3,0,1,1)
  }

  ## display
  filename <- paste0(par@loc, "/", par@level, "/", par@year, "/teams.html")
  cat(paste0("---\nlevel: ", par@level, "\nyear: ", par@year, "\nrel: ../../\n---\n<TABLE class=\"container\">\n"), file=filename)
  for (i in 1:3) {
    cat("<TR><TD align=\"center\">",names(standings)[i],"</TD><TD align=\"center\">",names(standings)[i+3],"</TD></TR>\n<TR><TD>", file=filename, append=TRUE)
    print(D[[i]], type="html", include.rownames=FALSE, html.table.attributes="class=\"sortable ctable\" width=100%", file=filename, append=TRUE, sanitize.text.function=I)
    cat("</TD>\n<TD>",file=filename,append=TRUE)
    print(D[[i+3]], type="html", include.rownames=FALSE, html.table.attributes="class=\"sortable ctable\" width=100%", file=filename, append=TRUE, sanitize.text.function=I)
    cat("</TD></TR>\n",file=filename,append=TRUE)
  }
  cat("</TABLE>\n",file=filename,append=TRUE)
  cleanTable(filename)
}

makeTeams.ncaa <- function(raw, team) {
  ## create list of matrices
  div <- levels(relevel(as.factor(conf$Conf), "Other"))
  n.d <- length(div)
  X <- D <- vector("list", n.d)
  PossG <- team$Poss
  PSP <- team$TmPTS/(team$Poss*team$G)
  PAG <- team$OpPTS/(team$Poss*team$G)
  Pct <- team$W/team$G
  names(PossG) <- names(PSP) <- names(PAG) <- names(Pct) <- team$Team
  for (d in 1:n.d) {
    tm <- rownames(subset(conf, Conf==div[d] & rownames(conf) %in% par@team))
    X[[d]] <- data.frame(team[tm]$W, team[tm]$L, Pct[tm], PossG[tm], PSP[tm], PAG[tm])

    ## Format for printing
    link <- character(nrow(X[[d]]))
    for (i in 1:nrow(X[[d]])) {
      link[i] <- paste0('<a href="', tm[i], '.html">', conf[tm[i],"Display"], '</a>')
    }
    X[[d]] <- cbind(link,X[[d]])
    colnames(X[[d]]) <- c("Team","W","L","Pct","Poss/G", "PS/P","PA/P")
    X[[d]] <- X[[d]][order(X[[d]]$Pct, decreasing=TRUE),]
    D[[d]] <- xtable(X[[d]])
    align(D[[d]]) <- c("l","l",rep("r",6))
    digits(D[[d]]) <- c(0,0,0,0,3,0,2,2)
  }

  ## display
  filename <- paste0(par@loc, "/", par@level, "/", par@year, "/teams.html")
  cat(paste0("---\nlevel: ", par@level, "\nyear: ", par@year, "\nrel: ../../\n---\n<TABLE class=\"container\">\n"), file=filename)
  for (r in 1:4) {
    i <- r + 1
    j <- if (r==3) 1 else r+5
    cat("<TR><TD align=\"center\">", div[i],"</TD>", file=filename, append=TRUE)
    if (j <= length(div)) {
      cat("<TD align=\"center\">", div[j], "</TD>", file=filename, append=TRUE)
    }
    cat("</TR>\n<TR><TD>", file=filename, append=TRUE)
    print(D[[i]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE, sanitize.text.function=I)
    cat("</TD>\n<TD>",file=filename,append=TRUE)
    if (j <= length(D)) {
      print(D[[j]], type="html", include.rownames=FALSE, html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE, sanitize.text.function=I)
    }
    cat("</TD></TR>\n",file=filename,append=TRUE)
  }
  cat("</TABLE>\n",file=filename,append=TRUE)
}

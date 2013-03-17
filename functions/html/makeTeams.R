makeTeams <- function(raw)
  {
    ## create list of matrices
    n.d <- length(raw$Standings)
    X <- D <- vector("list",n.d)
    for (d in 1:n.d)
      {
        X[[d]] <- raw$Standings[[d]]
        short.name <- raw$Team.Opp$Team[match(rownames(raw$Standings[[d]]),raw$Team.Opp$FullName)]
        long.name <- rownames(X[[d]])

        ## Format for printing
        link <- character(nrow(X[[d]]))
        for (i in 1:nrow(X[[d]]))
          {
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
    cat("<TABLE class=\"container\">\n",file=filename)
    for (i in 1:3)
      {
        cat("<TR><TD align=\"center\">",names(raw$Standings)[i],"</TD><TD align=\"center\">",names(raw$Standings)[i+3],"</TD></TR>\n<TR><TD>",file=filename,append=TRUE)
        print(D[[i]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE)
        cat("</TD>\n<TD>",file=filename,append=TRUE)
        print(D[[i+3]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%",file=filename,append=TRUE)
        cat("</TD></TR>\n",file=filename,append=TRUE)
      }
    cat("</TABLE>\n",file=filename,append=TRUE)
    cleanTable(filename)
  }

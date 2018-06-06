makeTeam <- function(raw, tf, vc, team.raw, type="html", teams=par@team, filename) {
  if (type=="pdf") {
    sink(filename)
    cat("\\documentclass[10pt,letterpaper]{article}\n")
    cat("\\usepackage{fullpage}\n")
    cat("\\usepackage[paperheight=14in,paperwidth=10in]{geometry}\n")
    cat("\\pagestyle{empty}\n")
    cat("\\setlength{\\textheight}{14in}\n")
    cat("\\setlength{\\voffset}{-1.5in}\n")
    cat("\\setlength{\\hoffset}{-0.75in}\n")
    cat("\\setcounter{topnumber}{3}\n")
    cat("\\setcounter{bottomnumber}{3}\n")
    cat("\\setcounter{totalnumber}{6}\n")
    cat("\\renewcommand{\\topfraction}{0.99}\n")
    cat("\\renewcommand{\\bottomfraction}{0.99}\n")
    cat("\\renewcommand{\\textfraction}{0.01}\n")
    cat("\\renewcommand{\\floatpagefraction}{0.98}\n")
    cat("\\begin{document}\n")
  }
  for (team in teams) {
    ##X.raw <- raw$Totals[raw$Totals[,"Team"]==team,]
    X.raw <- subset(raw, Team==team)
    X.tf <- subset(tf, Team==team)
    X.vc <- subset(vc, Team==team)
    X.tf[,Astd2 := 100*Astd2]
    X.tf[,Astd3 := 100*Astd3]
    ind <- which(substr(names(X.vc), 1, 2)=="VC")
    TmAdj <- par@poss/team.raw[team]$Poss
    for (v in which(substr(names(X.vc), 1, 2)=="VC")) {
      X.vc[[v]] <- X.vc[[v]] / 100 * (X.vc$TotalPoss / X.raw$G) * TmAdj
    }
    X.raw$USG <- calcUsage(X.raw)
    for (v in c("MP", "FG", "FGA", "3P", "3PA", "2P", "2PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")) {
      X.raw[[v]] <- X.raw[[v]]/X.raw$G
    }
    if (par@level=="ncaa") {
      X.raw$Yr[X.raw$Yr=="Freshman"] <- "Fr"
      X.raw$Yr[X.raw$Yr=="Sophomore"] <- "So"
      X.raw$Yr[X.raw$Yr=="Sophmore"] <- "So"
      X.raw$Yr[X.raw$Yr=="Junior"] <- "Jr"
      X.raw$Yr[X.raw$Yr=="Senior"] <- "Sr"
    }

    X.raw <- as.data.frame(X.raw)
    X.tf <- as.data.frame(X.tf)
    X.vc <- as.data.frame(X.vc)
    row.names(X.raw) <- X.raw$Name
    row.names(X.tf) <- X.tf$Name
    row.names(X.vc) <- X.vc$Name
    if (par@level=="ncaa") display.categories.raw <- c("Yr","No","Pos","G","MP","USG","FG","FGA","3P","3PA","FT","FTA","ORB","DRB","AST","TOV","STL","BLK","PTS")
    else {
      P <- as.matrix(X.raw[,c('pg','sg','sf','pf','c')])
      X.raw$Pos <- colnames(P)[apply(P, 1, which.max)]
      display.categories.raw <- c("Pos","G","MP","USG","FG","FGA","3P","3PA","FT","FTA","ORB","DRB","AST","TOV","STL","BLK","PTS")
    }
    display.categories.tf <- c("TotalPoss","Pts100","Pct1","Pct2","Pct3","eFG","TSP","Astd2","Astd3","Ast100","TO100","ORebPct","DRebPct","Stl100","Blk100")
    display.categories.vc1 <- c("VC.Ast","VC.TO","VC.1","VC.2","VC.3","VC.OReb","VC.DReb","VC.Stl","VC.Blk")
    display.categories.vc2 <- c("VC.Pass","VC.Sc","VC.Reb","VC.BkSt","VC.Off","VC.Def","VC.Ovr","WC")
    if (par@level=="ncaa") display.digits.raw <- c(0,0,0,0,0,rep(1,length(display.categories.raw)-4))
    else display.digits.raw <- c(0,0,0,rep(1,length(display.categories.raw)-2))

    display.digits.tf <- c(0,0,rep(1,7),2,2,2,1,1,2,2)
    display.digits.vc1 <- c(0,rep(1,length(display.categories.vc1)))
    display.digits.vc2 <- c(0,rep(1,length(display.categories.vc2)))
    ind.raw <- sort(X.raw[,"MP"],ind=T,dec=T)$ix
    ind.tf <- sort(X.tf[,"TotalPoss"],ind=T,dec=T)$ix
    ind.vc1 <- ind.vc2 <- sort(X.vc[,"WC"],ind=T,dec=T)$ix

    display.raw <- xtable(X.raw[ind.raw,display.categories.raw],digits=display.digits.raw)
    align(display.raw) <- rep("c",length(align(display.raw)))
    align(display.raw)[1] <- "l"
    display.tf <- xtable(X.tf[ind.tf,display.categories.tf],digits=display.digits.tf)
    align(display.tf) <- rep("c",length(align(display.tf)))
    align(display.tf)[1] <- "l"
    display.vc1 <- xtable(X.vc[ind.vc1,display.categories.vc1],digits=display.digits.vc1)
    align(display.vc1) <- rep("c",length(align(display.vc1)))
    align(display.vc1)[1] <- "l"
    display.vc2 <- xtable(X.vc[ind.vc2,display.categories.vc2],digits=display.digits.vc2)
    align(display.vc2) <- rep("c",length(align(display.vc2)))
    align(display.vc2)[1] <- "l"

    if (type=="html") {
      f <- paste0(par@loc, "/", par@level, "/", par@year, "/", team, ".html")
      cat(paste0("---\nlevel: ", par@level, "\nyear: ", par@year, "\nrel: ../../\n---\n"), file=f)
      if (par@level=="nba") cat(raw$Team.Opp$FullName[which(team==raw$Team.Opp$Team & raw$Team.Opp$Team.Opp=="Team")],"<br>\n", file=f, append=TRUE)
      if (par@level=="ncaa") cat("<h3>", conf[team,"Display"], "</h3><br>\n", file=f, append=TRUE)
      cat("<h4>Raw Statistics</h4>\n", file=f, append=TRUE)
      print(display.raw,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
      cat("<br>\n", file=f, append=TRUE)
      cat("<h4>Tempo-free Statistics</h4>\n", file=f, append=TRUE)
      print(display.tf,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
      cat("<br>\n", file=f, append=TRUE)
      cat("<h4>Value Created Statistics 1</h4>\n", file=f, append=TRUE)
      print(display.vc1,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
      cat("<br>\n", file=f, append=TRUE)
      cat("<h4>Value Created Statistics 2</h4>\n", file=f, append=TRUE)
      print(display.vc2,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
      cat("<br>\n", file=f, append=TRUE)
    } else if (type=="pdf") {
      cat("\\begin{center}\n")
      cat(team,"\n")
      cat("\\end{center}\n")
      ##cat("Raw Statistics\n")
      print(display.raw)
      ##cat("Tempo-free Statistics<br>\n")
      print(display.tf)
      ##cat("Value Created Statistics 1<br>\n")
      print(display.vc1)
      ##cat("Value Created Statistics 2<br>\n")
      print(display.vc2)
      cat("\\newpage\n")
    }
  }
  if (type=="pdf") {
    cat("\\end{document}\n")
    sink()
  }
}

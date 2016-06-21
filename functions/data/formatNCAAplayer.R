formatNCAAplayer <- function(team) {
  filename <- paste("data/ncaa/",par@year,"/raw/",team,".html",sep="")
  raw <- readHTMLTable(filename)
  Totals <- raw$totals
  Roster <- raw$roster
  Roster <- Roster[match(Totals$Player, Roster$Player),]
  val <- data.frame(team, Roster[,c("Player","#","Class")], Pos="", Roster[,c("Height","Weight")],
                    Totals[,c("G", "MP", "FG", "FGA", "3P", "3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PTS")])
  colnames(val)[1:7] <- c("Team","Name","No","Yr","Pos","Ht","Wt")
  colnames(val)[12:13] <- c('3P', '3PA')
  val$Yr <- tolower(val$Yr)
  #val$Ht[val$Ht==""] <- "6-5"
  #val$Wt[val$Wt==0] <- 209
  for (i in 7:(dim(val)[2])) val[,i] <- as.numeric(val[,i])
  val[val$MP > 0,]
}

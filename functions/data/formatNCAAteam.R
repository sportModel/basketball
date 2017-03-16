formatNCAAteam <- function(team) {
  filename <- paste("data/ncaa/",par@year,"/raw/",team,".html",sep="")
  raw <- readLines(filename)
  rec <- raw[grep('Overall', raw)][1]
  rec <- gsub(".* (\\d+-\\d+) .*", "\\1", rec)
  rec <- as.numeric(strsplit(rec, '-')[[1]])

  raw <- readHTMLTable(filename)
  val2 <- raw$team_stats[c(1,3), c("FG","FGA","FT","FTA","3P","3PA","PTS","ORB","DRB","AST","STL","TOV","BLK")]

  tmp <- cbind(G=raw$team_stats[c(1,3), "G"], W=rec, L=rev(rec), val2)
  val <- as.numeric(c(tmp[1,],tmp[2,]))
  names(val) <- c(paste0("Tm", colnames(tmp)), paste0("Op", colnames(tmp)))
  val
}

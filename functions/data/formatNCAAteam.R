formatNCAAteam <- function(team) {
  filename <- paste("data/ncaa/",par@year,"/raw/",team,".html",sep="")
  raw <- readLines(filename)
  rec <- raw[grep('Overall', raw)][1]
  rec <- gsub(".* (\\d+-\\d+) .*", "\\1", rec)
  rec <- as.numeric(strsplit(rec, '-')[[1]])
  w <- rec[1]
  l <- rec[2]

  raw <- readHTMLTable(filename)
  tmp <- raw$team_stats[c(1,3), c("FG","FGA","FT","FTA","3P","3PA","PTS","ORB","DRB","AST","STL","TOV","BLK")]

  for (j in 1:ncol(tmp)) tmp[,j] <- as.numeric(tmp[,j])
  Tm <- tmp[1,]
  Op <- tmp[2,]
  names(Tm) <- paste0("Tm", colnames(Tm))
  names(Op) <- paste0("Op", colnames(Op))
  out <- data.table(Team=team, G=as.numeric(raw$team_stats[1, "G"]), W=w, L=l, Tm, Op)
  out[, TmORBPct := TmORB/(TmORB+OpDRB)]
  out[, OpORBPct := OpORB/(OpORB+TmDRB)]
  out[, TmAstRate := TmAST/TmFG]
  out[, OpAstRate := OpAST/OpFG]
  out[, TmMiss := TmFGA - TmFG]
  out[, OpMiss := OpFGA - OpFG]
  TmPoss <- out$TmFGA-out$TmORBPct*(out$TmFGA-out$TmFG)*par@constants[1]+out$TmTOV+out$TmFTA*par@constants[2]
  OpPoss <- out$OpFGA-out$OpORBPct*(out$OpFGA-out$OpFG)*par@constants[1]+out$OpTOV+out$OpFTA*par@constants[2]
  out[, Poss := (TmPoss+OpPoss)/2/G]
  out
}

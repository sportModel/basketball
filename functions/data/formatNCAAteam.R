formatNCAAteam <- function(team) {
  filename <- paste0("data/ncaa/", par@year, "/raw/", team, ".html")
  pg <- xml2::read_html(filename)
  if (length(pg) < 2) stop(paste0("Missing HTML file for: ", team))
  
  # Parse record
  raw_p <- html_nodes(pg, 'p') %>% html_text()
  rec <- grep('Record', raw_p, value = TRUE) %>%
    str_replace("[^\\d]*(\\d+-\\d+).*(\\d+-\\d+).*", "\\1_\\2") %>% str_trim() %>%
    str_split('_') %>% .[[1]] %>% str_split('-') %>% .[[1]] %>% as.numeric()
  w <- rec[1]
  l <- rec[2]

  tmp <- html_nodes(pg, '#team_stats') %>% html_table() %>% .[[1]]
  tmp <- tmp[c(1,3), c("FG","FGA","FT","FTA","3P","3PA","PTS","ORB","DRB","AST","STL","TOV","BLK")]

  for (j in 1:ncol(tmp)) tmp[,j] <- as.numeric(tmp[,j])
  Tm <- tmp[1,]
  Op <- tmp[2,]
  names(Tm) <- paste0("Tm", colnames(Tm))
  names(Op) <- paste0("Op", colnames(Op))
  out <- data.table(Team=team, G=w+l, W=w, L=l, Tm, Op)
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

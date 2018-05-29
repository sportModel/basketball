formatNBAteam <- function(team, standings) {
  pg <- xml2::read_html(paste0("data/nba/", par@year, "/raw/", team, ".html"))
  if (length(pg) < 2) stop(paste0("Missing HTML file for: ", team))

  # Team name
  full.team.name <- html_nodes(pg, 'title') %>% as.character %>%
    stringr::str_replace(".*-[0123456789][0123456789] ", "") %>%
    stringr::str_replace(" Roster and.*", "") %>%
    stringr::str_squish()

  raw <- html_node(pg, '#team_and_opponent') %>% html_table()
  raw <- raw[match(c("Team","Opponent"),raw[,1]),-1]
  for (j in 1:ncol(raw)) raw[,j] <- as.numeric(raw[,j])
  Tm <- raw[1,-(1:2)]
  names(Tm) <- paste0("Tm", names(Tm))
  Op <- raw[2,-(1:2)]
  names(Op) <- paste0("Op", names(Op))
  df <- NULL
  nul <- lapply(standings,function(x){df <<- rbind(df,x)})
  w <- df$W[grep(full.team.name,rownames(df))]
  l <- df$L[grep(full.team.name,rownames(df))]
  out <- data.table(Team=team, FullName=full.team.name, raw[1, 1:2], W=w, L=l, Tm, Op)
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

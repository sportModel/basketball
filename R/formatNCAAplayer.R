formatNCAAplayer <- function(team) {
  pg <- xml2::read_html(paste0("data/ncaa/",par@year,"/raw/",team,".html"))
  if (length(pg) < 2) stop(paste0("Missing HTML file for: ", team))
  
  # Totals
  totals_raw <- html_node(pg, '#totals') %>% html_table()
  DT <- data.table(Team=team, Name=totals_raw[,2], totals_raw[,-(1:2)])
  setkey(DT, Name)
  
  # Roster
  roster_raw <- html_node(pg, '#roster') %>% html_table()
  roster <- data.table(roster_raw)[, .(Name=Player, No=`#`, Yr=tolower(Class), Ht=Height, Wt=Weight)]
  setkey(roster, Name)
  DT <- merge(DT, roster, all.x=TRUE)
  
  # Missing Ht/Wt (rare)
  DT[Ht=='', Ht := '6-6']
  DT[is.na(Wt), Wt := 200]
  
  DT[MP > 0 & Name != 'School Totals']
}

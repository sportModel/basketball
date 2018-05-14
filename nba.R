par <- setPar("nba", 2018)
nba.raw <- formatNBAdata()
# FIX THIS SO IT RETURNS A TEAM DT ALSO
nba.tf <- calcTF(nba.raw$Totals, nba.raw$Team.Opp$G)
nba.vc <- calcVC(nba.tf, nba.raw$Team.Opp)
save(nba.raw, nba.tf, nba.vc, file=paste("data/nba/2018/nba2018.rda"))
updateWebsite(nba.raw, nba.tf, nba.vc)

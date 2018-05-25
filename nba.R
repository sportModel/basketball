par <- setPar("nba", 2018)
nba.raw <- formatNBAdata()
# FIX THIS SO IT RETURNS A TEAM DT ALSO
nba.tf <- calcTF(nba.raw$Totals, nba.raw$Team.Opp$G)
nba.vc <- calcVC(nba.tf, nba.raw$Team.Opp)
save(nba.raw, nba.tf, nba.vc, file="data/nba/2018/nba2018.rda")
updateWebsite(nba.raw, nba.tf, nba.vc)

load('data/nba/2018/nba2018.rda')
VC <- nba.vc
VC[,grep('VC', names(VC))] <- round(VC[,grep('VC', names(VC))], 1)
VC <- data.table(VC)
VC[Team=="cle", c(2, 3, 10:26)][order(-WC)]
VC[Team=="bos", c(2, 3, 10:26)][order(-WC)]
VC[Team=="gsw", c(2, 3, 10:26)][order(-WC)]
VC[Team=="hou", c(2, 3, 10:26)][order(-WC)]

par <- setPar("nba", 2018)
nba.standings <- formatNBAstandings()
nba.team <- formatNBAteamData(nba.standings)
nba.raw <- formatNBAdata()
nba.tf <- calcTF(nba.raw, nba.team)
nba.vc <- calcVC(nba.tf, nba.team)
save(nba.standings, nba.team, nba.raw, nba.tf, nba.vc, file="data/nba/2018/nba2018.rda")
updateWebsite(nba.raw, nba.tf, nba.vc, nba.team, nba.standings)

load('data/nba/2018/nba2018.rda')
VC <- nba.vc
#cols <- grep('VC', names(VC), value=TRUE)
#VC[, (cols) := lapply(.SD, round, 1), .SDcols = cols]
VC[,grep('VC', names(VC))] <- round(VC[,grep('VC', names(VC))], 1)
VC <- data.table(VC)
VC[Team=="cle", c(2, 3, 10:26)][order(-WC)]
VC[Team=="bos", c(2, 3, 10:26)][order(-WC)]
VC[Team=="gsw", c(2, 3, 10:26)][order(-WC)]
VC[Team=="hou", c(2, 3, 10:26)][order(-WC)]

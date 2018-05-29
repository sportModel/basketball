updateWebsite <- function(raw, tf, vc, team, standings=NULL) {
  #browser()
  makePos(vc, team)
  if (par@level=="nba") makeTeams.nba(raw)
  if (par@level=="ncaa") makeTeams.ncaa(raw, team)
  makeTeam(raw, tf, vc, team.raw)
  #makeTeamComparison(team.raw)
}

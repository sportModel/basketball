updateWebsite <- function(raw, tf, vc, team, standings=NULL) {
  makePos(vc, team)
  if (par@level=="nba") makeTeams.nba(team, standings)
  if (par@level=="ncaa") makeTeams.ncaa(raw, team)
  makeTeam(raw, tf, vc, team)
  #makeTeamComparison(team.raw)
}

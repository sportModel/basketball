updateWebsite <- function(raw, tf, vc, team.raw) {
  makePos(vc, team.raw)
  if (par@level=="nba") makeTeams.nba(raw)
  if (par@level=="ncaa") makeTeams.ncaa(raw, team.raw)
  makeTeam(raw, tf, vc, team.raw)
  makeTeamComparison(team.raw)
}

updateWebsite <- function(raw,tf,vc)
  {
    makePos(vc)
    if (par@level=="nba") makeTeams(raw)
    makeTeam(raw,tf,vc)
  }

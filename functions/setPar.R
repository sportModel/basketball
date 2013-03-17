setClass("par",representation(level="character",
                              team="character",
                              year="numeric",
                              model="list",
                              min.game="numeric",
                              constants="numeric",
                              loc="character"))

setPar <- function(level,year)
  {
    team <- setTeam(level,year)
    if (level=="nba")
      {
        min.game <- 48
        constants <- c(1.07,0.4,100)
      }
    if (level=="ncaa")
      {
        min.game <- 40
        constants <- c(1.07,0.475,100)
      }
    names(constants) <- c("gamma1","gamma2","gamma3")
    year <- year
    load(file="~/spt/basketball/model/model.RData")
    val <- new("par",
               level=level,
               team=team,
               model=model,
               year=year,
               min.game=min.game,
               constants=constants,
               loc="~/sports/web/basketball")
    val
  }

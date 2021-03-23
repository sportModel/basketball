setClass("par",representation(level="character",
                              team="character",
                              year="numeric",
                              model="list",
                              poss="numeric",
                              min.game="numeric",
                              constants="numeric",
                              loc="character"))

setPar <- function(level, year) {
  team <- setTeam(level, year)
  if (level=="nba") {
    min.game <- 48
    constants <- c(1.07, 0.4, 100)
    poss <- 100
  }
  if (level=="ncaa") {
    min.game <- 40
    constants <- c(1.07, 0.475, 100)
    load("model/clust.RData", envir=.GlobalEnv)
    conf <- read.delim("data/ncaa/conference.txt")
    rownames(conf) <- conf$Link
    conf$Conf <- conf[, paste0("X",year)]
    assign("conf", conf[,c("Display", "Conf")], envir=.GlobalEnv)
    poss <- 67
  }
  names(constants) <- c("gamma1","gamma2","gamma3")
  year <- year
  load(file="~/spt/basketball/model/model.RData")
  val <- new("par",
             level=level,
             team=team,
             model=model,
             poss=poss,
             year=year,
             min.game=min.game,
             constants=constants,
             loc="~/spt/basketball/web")
  val
}

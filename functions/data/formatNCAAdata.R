formatNCAAdata <- function() {
  player <- NULL
  for (i in par@team) {
    player.i <- formatNCAAplayer(i)
    pos.i <- assignPos(player.i)
    Pos.i <- colnames(pos.i)[apply(pos.i,1,which.max)]
    player.i <- data.table(Name=player.i$Name, Team=i, Pos=Pos.i, pos.i, player.i[,-(1:2)])
    player <- rbind(player,player.i)
  }
  player
}

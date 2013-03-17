printSummary <- function(boxes,game.poss)
  {
    cat("Possessions: ",round(game.poss),"\n",sep="")
    team.names <- names(boxes)
    n <- nchar(team.names)
    pad <- max(n) + 3 - n
    score <- c(sum(boxes[[1]]$PTS),sum(boxes[[2]]$PTS))
    ppp <- score/game.poss
    cat("\n",team.names[1],rep(" ",pad[1]),score[1],rep(" ",5),ppp[1],"\n",sep="")
    cat(team.names[2],rep(" ",pad[2]),score[2],rep(" ",5),ppp[2],"\n\n",sep="")
  }

expDiff <- function(ff1,ff2,poss)
  {
    diff <- ff2-ff1
    diff[1] <- diff[1]*poss
    diff[2] <- poss*diff[2]/2
    diff[3] <- -1*diff[3]
    val <- sum(diff)
    val
  }

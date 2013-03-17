formatName <- function(x)
  {
    val <- NULL
    for (i in 1:length(x))
      {
        player <- unlist(strsplit(x[i]," "))
        if (length(player)!=2)
          {
            player <- formatLongName(player)
          }
        else
          {
            player <- c(player[2],player[1])
          }
        val <- rbind(val,player)
      }
    colnames(val) <- c("LastName","FirstName")
    rownames(val) <- NULL
    return(val)
  }

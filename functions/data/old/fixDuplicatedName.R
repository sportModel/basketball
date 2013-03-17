fixDuplicatedName <- function(x,team)
  {
    val <- x
    if (paste(x,collapse=" ") == "Ryan Anderson" & team == "nebraska") val <- c("Ryan","Anderson 1")
    if (paste(x,collapse=" ") == "Ryan Anderson" & team == "california") val <- c("Ryan","Anderson 2")
    if (paste(x,collapse=" ") == "Larry Davis" & team == "cincinnati") val <- c("Larry","Davis 1")
    if (paste(x,collapse=" ") == "Larry Davis" & team == "setonhall") val <- c("Larry","Davis 2")
    if (paste(x,collapse=" ") == "Mike Davis" & team == "illinois") val <- c("Mike","Davis 1")
    if (paste(x,collapse=" ") == "Mike Davis" & team == "setonhall") val <- c("Mike","Davis 2")
    return(val)
  }

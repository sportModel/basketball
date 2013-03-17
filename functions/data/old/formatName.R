formatName <- function(x,team)
  {
    n <- length(x)
    val <- matrix("",ncol=2,nrow=n)
    for (i in 1:n)
      {
        a <- unlist(strsplit(x[i]," "))
        a <- a[a!=""]
        if (length(a) > 2) a <- fixLongName(a)
        a <- fixDuplicatedName(a,team)
        if (length(grep(",",a))==0) val[i,] <- a[2:1]
        else val[i,] <- gsub(",","",a)
      }
    colnames(val) <- c("last.name","first.name")
    val
  }

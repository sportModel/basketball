cut <- function(x)
  {
    n <- length(x)
    for (i in 1:n)
      {
        a <- unlist(strsplit(x[i]," "))
        x[i] <- paste(a[a!=""],collapse=" ")
      }
    x
  }

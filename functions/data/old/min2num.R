min2num <- function(x)
  {
    n <- length(x)
    A <- matrix(as.numeric(unlist(strsplit(x,":"))),nrow=n,ncol=2,byrow=T)
    val <- A[,1] + A[,2] / 60
    val
  }

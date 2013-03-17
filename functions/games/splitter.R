splitter <- function(x)
  {
    return(matrix(unlist(strsplit(x,"-")),ncol=2,byrow=T))
  }

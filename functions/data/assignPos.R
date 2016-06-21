assignPos <- function(player) {
  n <- nrow(player)
  min <- player$MP
  reb <- player$DRB/min
  ast <- player$AST/min
  ht <- matrix(as.numeric(unlist(strsplit(player$Ht,"-"))),ncol=2,byrow=T)
  ht <- 12*ht[,1]+ht[,2]
  wt <- player$Wt
  
  val <- matrix(0,nrow=n,ncol=5,dimnames=list(NULL,c("pg","sg","sf","pf","c")))
  for (i in 1:n) {
    ##par(mfrow=c(5,4),oma=c(0,0,1,0))
    ##x <- c(ht[i],wt[i],reb[i],ast[i])
    for (j in 1:5) {
      val[i,j] <- dnorm(ht[i]-PosClust$centers[j,"ht"],sd=SDclust[j,"ht"])*
        dnorm((wt[i]-PosClust$centers[j,"wt"])/2,sd=SDclust[j,"wt"])*
        ##              dnorm(wt[i]-PosClust$centers[j,"wt"],sd=SDclust[j,"wt"])*
        dnorm(reb[i]-PosClust$centers[j,"reb"],sd=SDclust[j,"reb"])*
        dnorm(ast[i]-PosClust$centers[j,"ast"],sd=SDclust[j,"ast"])
      ##            for (k in 1:4)
      ##              {
      ##                m <- PosClust$centers[j,k]
      ##                s <- SDclust[j,k]
      ##                xx <- seq(m-3*s,m+3*s,len=51)
      ##                if (k==1) ylab <- colnames(val)[j]
      ##                else ylab <- ""
      ##                plot(xx,dnorm(xx,mean=m,sd=s),type="l",xlab=colnames(PosClust$centers)[k],ylab=ylab,main=round(10*dnorm(x[k],mean=m,sd=s)/dnorm(m,mean=m,sd=s),3))
      ##                abline(v=x[k],col="red",lwd=2)
      ##              }
    }
    ##        mtext(player$Name[i], outer = TRUE, cex = 1, line=-1)
  }
  conv <- FALSE
  counter <- 0
  for (i in 1:n) val[i,] <- val[i,]/sum(val[i,])
  while (!conv) {
    counter <- counter+1
    ##print(counter)
    old <- val
    for (j in 1:5) val[,j] <- val[,j]/sum(min*val[,j])
    for (i in 1:n) val[i,] <- val[i,]/sum(val[i,])
    if (max(val-old) < .001) conv <- TRUE
  }
  
  val <- round(val,digits=2)
  return(val)
}

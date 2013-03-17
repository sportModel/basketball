load("clust.RData")
par <- setPar("ncaa",2012)
ncaa.raw <- formatNCAAdata()

min <- ncaa.raw$MP
reb <- ncaa.raw$DRB/min
ast <- ncaa.raw$AST/min
ht <- matrix(as.numeric(unlist(strsplit(ncaa.raw$Ht,"-"))),ncol=2,byrow=T)
ht <- 12*ht[,1]+ht[,2]
wt <- ncaa.raw$Wt

X <- cbind(ht,wt,reb,ast)
Xc <- rbind(c(72,185,.07,.11),c(75,185,.08,.08),c(77.5,205,.12,.065),c(80,225,.145,.04),c(81.5,250,.16,.04))
##kmeans(X,5)$centers
##kmeans(X,Xc)$centers
PosClust <- kmeans(X,Xc)
SDclust <- rbind(apply(X[PosClust$cluster==1,],2,sd),
                 apply(X[PosClust$cluster==2,],2,sd),
                 apply(X[PosClust$cluster==3,],2,sd),
                 apply(X[PosClust$cluster==4,],2,sd),
                 apply(X[PosClust$cluster==5,],2,sd))
## Equalize weight for PG/SG, ast for PF/C
PosClust$centers[1:2,"wt"] <- mean(X[PosClust$cluster < 3,"wt"])
SDclust[1:2,"wt"] <- sd(X[PosClust$cluster < 3,"wt"])
PosClust$centers[4:5,"ast"] <- mean(X[PosClust$cluster > 3,"ast"])
SDclust[4:5,"ast"] <- sd(X[PosClust$cluster > 3,"ast"])

save(PosClust,SDclust,file="clust.RData")

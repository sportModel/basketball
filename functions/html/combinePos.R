combinePos <- function(vc,Pos)
  {
    ## Row names
    X <- vc[is.element(vc[,"Pos"],Pos),]
    id <- apply(X[,c("first.name","last.name")],1,paste,collapse=" ")
    dup <- grep("NA",row.names(X))
    if (length(dup)!=0)
      {
        X.combined <- X[-dup,]
        id.combined <- id[-dup]
        for (i in 1:length(dup))
          {
            id1 <- dup[i]
            id2 <- which(id.combined==id[id1])
            X.combined[id2,"team"] <- paste(X.combined[id2,"team"],X[id1,"team"],sep="/")
            TotalPoss <- X.combined[id2,"TotalPoss"] + X[id1,"TotalPoss"]
            X.combined[id2,6:21] <- (X.combined[id2,"TotalPoss"]/TotalPoss) * X.combined[id2,6:21] + (X[id1,"TotalPoss"]/TotalPoss) * X[id1,6:21]
            X.combined[id2,"WC"] <- X.combined[id2,"WC"] + X[id1,"WC"]
            X.combined[id2,"TotalPoss"] <- TotalPoss
          }
      }
    else
      {
        X.combined <- X
        id.combined <- id
      }
    row.names(X.combined) <- id.combined
    X.combined <- X.combined[,-1:-2]
    X.combined
  }

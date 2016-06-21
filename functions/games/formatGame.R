## ESPN
## Must use mozilla to download; IE doesn't record tabs correctly
formatGame <- function(filename) {
  raw <- read.delim(filename,header=F)
  STARTERS <- grep("STARTERS",raw[,1])
  BENCH <- grep("BENCH",raw[,1])
  TOTALS <- grep("TOTALS",raw[,1])
  team.names <- raw[STARTERS-1,1]
  raw1 <- raw[c((STARTERS[1]+1):(BENCH[1]-1),(BENCH[1]+1):(TOTALS[1]-1)),]
  raw2 <- raw[c((STARTERS[2]+1):(BENCH[2]-1),(BENCH[2]+1):(TOTALS[2]-1)),]
  raw1 <- raw1[raw1[,5]!="",]
  raw2 <- raw2[raw2[,5]!="",]
  n1 <- nrow(raw1)
  n2 <- nrow(raw2)
  raw <- rbind(raw1,raw2)
  tmp <- matrix(unlist(strsplit(raw[,1],",")),ncol=2,byrow=T)
  Name <- tmp[,1]
  Pos <- gsub(" ","",tmp[,2])
  
  pos <- matrix(0,ncol=5,nrow=n1+n2)
  pos[Pos=="G",1:2] <- .5
  pos[Pos=="F",3:4] <- .5
  pos[Pos=="F-C",5] <- 1
  pos[Pos=="G-F",2:3] <- .5
  pos[Pos=="PG",1] <- 1
  pos[Pos=="SG",2] <- 1
  pos[Pos=="SF",3] <- 1
  pos[Pos=="PF",4] <- 1
  pos[Pos=="C",5] <- 1
  
  if (par@level=="ncaa") {
    raw <- cbind(Name,Team=c(rep(1,n1),rep(2,n2)),Pos,pos,"","","","",1,NA,raw[,2],splitter(raw[,3]),splitter(raw[,4]),splitter(raw[,5]),raw[,6],raw[,7],raw[,9],raw[,12],raw[,10:11],raw[,14])
    colnames(raw) <- c("Name","Team","Pos","pg","sg","sf","pf","c","No","Yr","Ht","Wt","G","GS","MP","FG","FGA","3P","3PA","FT","FTA","ORB","DRB","AST","TOV","STL","BLK","PTS")
    raw <- as.data.frame(raw)
  }
  if (par@level=="nba") {
    raw <- cbind(Name,Team=c(rep(1,n1),rep(2,n2)),Pos,pos,1,raw[,2],splitter(raw[,3]),splitter(raw[,4]),splitter(raw[,5]),raw[,6],raw[,7],raw[,9],raw[,12],raw[,10],raw[,11],raw[,15])
    colnames(raw) <- c("Name","Team","Pos","pg","sg","sf","pf","c","G","MP","FG","FGA","3P","3PA","FT","FTA","ORB","DRB","AST","TOV","STL","BLK","PTS")
    raw <- as.data.frame(raw)
  }
  for (i in 4:ncol(raw)) raw[,i] <- as.numeric(raw[,i])
  
  team1 <- raw[1:n1,]
  team2 <- raw[(n1+1):(dim(raw)[1]),]
  val <- list(team1=team1,team2=team2)
  names(val) <- team.names
  return(val)
}

formatLongName <- function(x)
  {
    if (x[3]=="Jr.") return(c(paste(x[2],x[3]),x[1]))
    if (x[3]=="II") return(c(paste(x[2],x[3]),x[1]))
    if (x[3]=="III") return(c(paste(x[2],x[3]),x[1]))
    name <- paste(x,collapse=" ")
    ##print(name)
    ##if (name=="") return(c("",""))
    if (name=="Juan Carlos Navarro") return(c("Navarro","Juan Carlos"))
    if (name=="Ricardo De Bem") return(c("De Bem","Ricardo"))
    if (name=="Luc Richard Mbah a Moute") return(c("Mbah a Moute","Luc Richard"))
    if (name=="John Michael Hall") return(c("Hall","John Michael"))
    if (name=="Max Paulhus Gosselin") return(c("Paulhus Gosselin","Max"))
    if (name=="Metta World Peace") return(c("World Peace, Metta"))
  }

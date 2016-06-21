par <- setPar("nba",2012)
nba.raw <- formatNBAdata()
nba.tf <- calcTF(nba.raw$Totals,nba.raw$Team.Opp[nba.raw$Team.Opp$Team.Opp=="Team","G"])
w <- nba.tf$TotalPoss
y <- 100*nba.raw$Totals$Astd
a <- nba.tf$Ast100
s <- 100*nba.raw$Totals[,"FGA"]/nba.tf$TotalPoss
a <- a-mean(a)
s <- s-mean(s)
summary(fit <- lm(y~a+s, weights=w))
astd <- coef(fit)
save(astd, file="astd.RData")

## Look
plot(s,y, pch=19, cex=sqrt(w)/100)
identify(s,y,labels=nba.raw$Totals$Name)

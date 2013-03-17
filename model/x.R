load("Data.RData")
attach(Data)

## Formatting; Possessions
Poss1 <- A+.5*A1+TO-Off
Poss2 <- A.o+.5*A1.o+TO.o-Off.o
Poss <- (Poss1+Poss2)/2
A2 <- A-A3
M2 <- M-M3
Pct2 <- M2/A2
Blk100 <- 100*(Blk/Poss)
Stl100 <- 100*(Stl/Poss)
DRebPct <- Def/(A.o-M.o)
DReb100 <- 100*Def/Poss
ADReb100 <- DRebPct*mean(100*(A-M)/Poss)

## Expectations
eP <- mean(Pts/Poss)
eR <- mean(Off/(A-M))
eM100 <- mean(100*(A-M)/Poss)
eStl100 <- mean(Stl100)
eBlk100 <- mean(Blk100)
eADReb100 <- mean(ADReb100)
eR.FT <- .05

## Value of an extra point per 100 Possessions
dP <- (Pts-Pts.o)/Poss
dP100 <- dP*100
dWP <- WLPct-.5
mod <- lm(dWP~0+dP100)
summary(mod)
P100.value <- mod$coefficients

## Value of steals, blocks and defensive rebounds
dStl100 <- Stl100-eStl100
dBlk100 <- Blk100-eBlk100
dADReb100 <- ADReb100-eADReb100
dDReb100 <- DReb100-mean(DReb100)
dDefP100 <- 100*(eP-Pts.o/Poss)
mod <- lm(dDefP100~0+dBlk100+dStl100+dADReb100)
summary(mod)
def.coef <- mod$coefficients
weight <- summary(mod)$r.squared

model <- list(eP=eP,
              eR=eR,
              eR.FT=eR.FT,
              eM100=eM100,
              eStl100=eStl100,
              eBlk100=eBlk100,
              eADReb100=eADReb100,
              p100.value=P100.value,
              dBlk100=def.coef["dBlk100"],
              dStl100=def.coef["dStl100"],
              dADReb100=def.coef["dADReb100"],
              dRebValue=1,
              weight=weight)
save(model,file="model.RData")

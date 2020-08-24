library(tidyverse)
library(MASS)

png(  filename="dice.png"
    , width = 1920
    , height = 1080)

op <- par(mfcol=c(2,1))

DiceProb <- c(1/9,rep(1/6,4),2/9)

w1 <- sample(1:6,100,replace=TRUE, prob = DiceProb)
w2 <- sample(1:6,1000,replace=TRUE, prob = DiceProb)

s = sqrt(5/36/100)

print (s)

barplot( table(w1)/100
  , main="Test mit 100 Würfen"
  , xlab = "Augen"
  , ylim = c(0,0.3)
)

abline ( h = 1/6 -1.96*s)
abline ( h = 1/6 +1.96*s)

s = sqrt(5/36/1000)

barplot( table(w2)/1000
         , main= "Spiel mit 1.ooo Würfen"
         , xlab = "Augen"
         , ylim = c(0,0.3)
         )
abline ( h = 1/6 -1.96*s)
abline ( h = 1/6 +1.96*s)

dev.off()
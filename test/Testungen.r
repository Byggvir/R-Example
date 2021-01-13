library(tidyverse)
library(MASS)

png(  filename="/tmp/Testungen.png"
    , width = 1920
    , height = 1080)

op <- par(mfcol=c(2,1))

DiceProb <- c(0.05,0.95)

dice <- function (n, t) {

  l <- length(DiceProb)  
  w <- NA
  w <- sample(1:l,n,replace=TRUE, prob = DiceProb)
  s = sqrt(DiceProb[1]*DiceProb[2]/n)
  
  bp <- barplot( table(w)/n*100
  , main = t
  , xlab = "Augen %"
  , ylab = "Würfe [%]"
  , ylim = c(0,100)
  )

  text( x = bp 
      , y = 0
      , labels = table(w)
      , pos = 3
      , cex = 2
      , col = "black"
      
  )

  rm (bp)
  
}

dice(1000000, "Test mit 100 Würfen")
dice(5000000, "Spiel mit 1000 Würfen")

dev.off()

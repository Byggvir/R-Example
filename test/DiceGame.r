library(tidyverse)
library(MASS)

png(  filename="dice.png"
    , width = 1920
    , height = 1080)

op <- par(mfcol=c(2,1))

DiceProb <- c(1/9,rep(1/6,4),2/9)

dice <- function (n, t) {
  
  w <- NA
  w <- sample(1:6,n,replace=TRUE, prob = DiceProb)
  s = sqrt(5/36/n)

  bp <- barplot( table(w)/n*100
  , main = t
  , xlab = "Augen %"
  , ylab = "Würfe [%]"
  , ylim = c(0,30)
  )

  text( x = bp 
      , y = 0
      , labels = table(w)
      , pos = 3
      , cex = 2
      , col = "black"
      
  )
  abline ( h = ( 1/6 - 1.96 * s ) * 100 )
  abline ( h = ( 1/6 + 1.96 * s ) * 100 )
  
  rm (bp)
  
}

dice(100, "Test mit 100 Würfen")
dice(1000, "Spiel mit 1000 Würfen")

dev.off()

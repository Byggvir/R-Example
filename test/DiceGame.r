library(tidyverse)
library(MASS)

png(  filename="/tmp/dice.png"
    , width = 1920
    , height = 1080)

op <- par(mfcol=c(2,1))

DiceProb <- c(1/9,rep(1/6,4),2/9)

dice <- function (n, t) {
  
  w <- NA
  w <- sample(1:6,n,replace=TRUE, prob = DiceProb)
  s = sqrt(5/36/n)
  
  print(c(mean(w),sd(w),sd(1:6),sd(1:6)/sqrt(n)))
  
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
  abline ( h = ( 1/6 - 1.95996398454005 * s ) * 100, col='green')
  abline ( h = ( 1/6 + 1.95996398454005 * s ) * 100, col='green')
  abline ( h = ( 1/6 - 2.5758293035489 * s ) * 100, col='red')
  abline ( h = ( 1/6 + 2.5758293035489 * s ) * 100, col='red' )
  
  rm (bp)
  
}

dice(341, "Test mit 341 Würfen")
dice(1000, "Spiel mit 1000 Würfen")

dev.off()

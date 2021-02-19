#!/usr/bin/env Rscript

setwd("~/git/R-Example")

source("lib/copyright.r")

png( "Kurven06.png"
     , width = 1920
     , height = 1080)

a <- 1
b <- 240

mycolors <- rainbow(7)

A <- 1
dT <- 1/60
ylim <- c(0,2*A)

plot( NA
     , xlim = c(a,b)
     , ylim = ylim
     , ylab = "Virenlast"
)

Filterleistung <- c(0.5,1,2,3,4,5,6)

for (i in 1:7 ){

FL <- Filterleistung[i]

Viren[1] <- A*dT

for (j in (a+1):b) {
  
  Viren[j] <- Viren[j-1] + A*dT - Viren[j-1] * FL * dT
  
}

print(Viren)
lines( a:b 
    , Viren
    , col = mycolors[i]
    , lwd = 5
    , type = "l"

    )

}

title (
  main = 'Reinigung eines Klassenraumes durch Filter'
  , cex.main = 4
  , sub = "Reduzierung der Viren nach der Zeit'"
)
legend(
  "topleft"
  , title = 'Raumvolumen 150m³, Filterleistung'
  , legend = paste(Filterleistung*150, 'm³/h')
  , col = mycolors
  , lty = c(1,1)
  , lwd = 5
  , cex = 2
  , inset = 0.1
)

grid()

copyright(holders = c('TAr'))

dev.off()


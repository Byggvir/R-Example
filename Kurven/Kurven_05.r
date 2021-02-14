#!/usr/bin/env Rscript

setwd("~/git/R-Example")

source("lib/copyright.r")

png( "Kurven05.png"
     , width = 1920
     , height = 1080)

a <- 0
b <- 1

V <- 150
FL <- 70

s <- function (x,V, FL) {

  return (1-exp(-FL/V*x))

}

ylim <- c(0,100)
mycolors <- rainbow(7)
FL <- c(0.5,1,2,3,4,5,6)
plot(NULL, 
     xlim = c(a,b)
     , ylim = ylim
     )

for ( i in 1:7 ) {
  
curve( s(x,150,FL[i]*150) * 100
    , a, b
    , ylim = ylim
    , col=mycolors[i]
    , xlab = "Zeit [h]"
    , ylab="Volumen %"
    , lwd = 5

    )

par(new=TRUE)

}
title (
  main = 'Reinigung eines Klassenraumes durch Filter'
  , cex.main = 4
  , sub = "Reduzierung der Schadstoffe nach der Zeit'"
)
legend(
  "topright"
  , title = 'Raumvolumen 150m³, Filterleistung'
  , legend = paste(FL*150, 'm³/h')
  , col = mycolors
  , lty = c(1,1)
  , lwd = 5
  , cex = 3
  , inset = 0.1
)

grid()

copyright(holders = c('TAr'))

dev.off()

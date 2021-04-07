#!/usr/bin/env Rscript

setwd("~/git/R-Example")

source("lib/copyright.r")

png( "Kurven07.png"
     , width = 1920
     , height = 1080)

a <- 0
b <- 30

m <- 210
o <-873

mycolors <- c("red","green","blue")

curve(m*exp(0.08065*x)
      , a , b
      , col = mycolors[1]
      , xlab=""
      , ylab=""
      , ylim = c(0,3000)
      , lwd = 3
)

par(new = TRUE)

curve((o-m)*exp(-0.04014*x)
      , a , b
      , col = mycolors[2]
      , xaxt="n"
      , yaxt="n"
      , xlab=""
      , ylab=""
      , ylim = c(0,3000)
      , lwd = 3
    )

par(new = TRUE)

curve(o*exp(-0.04014*x) + m*exp(0.08065*x)
      , a , b
      , col = mycolors[3]
      , xaxt="n"
      , yaxt="n"
      , xlab=""
      , ylab=""
      , ylim = c(0,3000)
      , lwd = 3
)
title (
  main = 'Alte / Neue Varianten Baden-Württemberg'
  , cex.main = 4
  , sub = "Verdrängung"
)
legend(
  "topleft"
  , title = 'Variante'
  , legend = c('VoC', 'Alt', "Summe")
  , col = mycolors
  , lty = c(1,1)
  , lwd = 5
  , cex = 2
  , inset = 0.1
)

grid()

copyright(holders = c('TAr'))

dev.off()


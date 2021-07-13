#!/usr/bin/env Rscript

library(data.table)
library(readODS)

setwd("~/git/R-Example")

png(filename = "png/Lebenserwartung.png"
    , width = 1080
    , height = 1080
    )

par(  mar = c(10,10,10,10) 
      , bg = rgb(0.95,0.95,0.95,1)
)

Lebenswerwartung <- read_ods( 
  "data/12621-0002.ods"
  , sheet = 1
  )

plot( Lebenswerwartung$Age
  , Lebenswerwartung$Male  # + Lebenswerwartung$Age
  , type = "l"
  , col = "blue"
  , main = "Lebenserwartung in DEU"
  , xlab = "Alter"
  , ylab = "Restlebenserwartung"
  , xlim = c(50,105)
  , ylim = c(0,55)
  , cex.axis = 2
  , cex.main = 4
  , cex.sub = 2
  , cex.lab = 1.5
  , lwd = 5
  )

title ( sub = "Restlebenserwartung nach erreichtem Alter in Jahren"
        , line = 5
        , cex.sub = 3
)

lines( Lebenswerwartung$Age
  , Lebenswerwartung$Female # + Lebenswerwartung$Age
  , type = "l"
  , col = "green"
  , main = "Lebenserwartung"
  , lwd = 5
  )

abline( v = Lebenswerwartung$Male[1]
        , col = "blue"
        , lty = 2
        , lwd = 5
)

abline( v = Lebenswerwartung$Female[1]
       , col = "green"
       , lty = 2
       , lwd = 5
       )

legend ( "bottomleft"
  , legend = c("Frauen", "Männer")
  , col = c("green","blue")
  , cex = 3
  , lty = 1
  , lwd = 5
  , inset = 0.05
  
)
grid()

dev.off()

#!usr/bin/env Rscript
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#
MyScriptName <-"SampleSize"
PNG <- "png/SampleSize"

setwd("~/git/R-Example")

source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

png( paste( PNG, '.png', sep="" )
     , width=1920
     , height=1080
)
par( mar = c(10,5,10,5)
)

# plot( NA
#      , NA
#      , xlim=c(0,100)
#      , ylim=c(0,1000000)
# )

g<-0.1
curve( (1.96/g)^2*(100000-x)/x
      , from = 20
      , to   = 200
      , xlim =c(0,200)
      , ylim = c(0,2000000)
      , xlab = "Inzidenz I"
      , ylab = "400*(100000-I)/I)"
      , lwd = 3
      , col = "blue"
)

title ( 
  main = paste("Stichprobengröße um die Inzidenz\nauf",g*100,"% genau zu bestimmen")
  , line= 3
  , cex.main = 4
)

title ( 
  sub = paste("Neue Fälle pro Woche pro 100.000 Einwohner")
  , line= 5
  , cex.sub = 2
)

grid()

copyright('TAr')

dev.off()

x <- c(35,50,100,200)

print((1.96/g)^2*(100000-x)/x)

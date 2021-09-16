#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "Mutationen"

setwd("~/git/R-Example")

source("lib/copyright.r")
source("lib/myfunctions.r")

require(data.table)
require(date)
library(readODS)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)

png( paste( "png/RKI_R-Zahl"
            , ".png"
            , sep = ""
  )
  , width = 1920
  , height = 1080
)

par(  mar = c(10,5,10,5) 
    , bg = rgb(0.95,0.95,0.95,1)
    , mfcol = c(1,1))


rzahlen <- function ( data ) {
  
  ylim <- c(0,2)
  
  plot( 
         as.Date(data[,1])
       , data[,8]
       , main = ""
       , sub = ""
       , xlab = ""
       , ylab = ""
       , ylim = ylim
       , type = "l"
       , lwd = 5
       , col = "blue"
  )

  abline (h=0.75, col="green")
  abline (h=0.90, col="orange")
  abline (h=1.00, col="red")

  title (
      main = "R-Zahlen in DEU nach RKI"
    , cex.main = 4
    , line = 4
    
  )
  title (
    sub = "7-Tage-R"
  , cex.sub = 3
  , line = 8
  
  )
  title (
    xlab = "Datum"
    , cex.lab = 2

  )
  title (
    ylab = "R-Zahl"
    , cex.lab = 2
    
  )
  legend( "topright"
    , legend = c("7-Tage-R")
    , col = c("blue") 
    , lwd = 5
    , cex = 3
         )
  grid()

}

daten <- read.csv( file='data/Nowcasting.csv')

rzahlen(daten[5:nrow(daten),])

dev.off()


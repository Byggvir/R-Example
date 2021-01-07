#!usr/bin/env Rscript
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"Curve"

library(tidyverse)
library(lubridate)
library(REST)

require(data.table)


setwd("~/git/R-Example")

source("lib/copyright.r")

png(
  paste(
    MyScriptName
     , ".png"
    , sep = ""
  )
  , width = 1920
  , height = 1080
)

a <- c(0,0,0);
b <- c(1.1,1.2,1.3)
linecol <- c(
  "green"
  ,"yellow"
  , "red"
)

xlim <- c(0,5)


for (i in 1:3 ) {
 
  curve( exp(a[i]+b[i]*x)
       , from =0
       , to = 3
       , col = linecol[i]
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , xlim = c(0,3)
       , ylim = c(0,exp(max(b)*3))
       , lwd = 3
       , lty = 3

)
  grid()
  par (new = TRUE)
}
 dev.off()
 

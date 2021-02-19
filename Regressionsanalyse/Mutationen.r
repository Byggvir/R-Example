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

require(data.table)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)

setwd("~/git/R-Example")

source("lib/copyright.r")
source("common/ta_regressionanalysis.r")
source("common/rki_sql.r")

CI <- 0.95

regression_analysis <- function ( x , y) {
  
  ra <- lm(log(y) ~ x)
  ci <- confint(ra,level = CI)
  
  print(ra)
  
  print(ci)
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  print(round(b[2]*exp(a[2]+b[2]*(0:10))))
  print(y[2:11]-y[1:10])
  xlim <- c(min(x),max(x))
  ylim <- c(  0
              , ( max(
                c(   exp(a)
                     , exp(a+b*max(x))
                )
              ) %/% 100 + 1 ) * 100
  )
  
  png( paste( "png/RA_Mutationen"
              , ".png"
              , sep = ""
  )
  , width = 1080
  , height = 1080
  )
  
  par (   mar = c(10,5,10,5) 
          , bg = "lightyellow")
  
  plot(  x
       , y
       , main = ""
       , sub = ""
       , xlab = "Tag x"
       , ylab = "Anzahl"
       , ylim = ylim
       , type = "l"
       , lwd = 3
       , xlim = xlim
       , col = "black"
       
  )
  t <- title ( 
    main = "Mutationen Baden-Württemberg"
    , cex.main = 3
  )
  t <- title ( 
      sub = "Kumulierte Fälle ab 08.02.2021"
    , cex.sub = 2
    , line = -3
  )
  
  
  copyright(c("TA"))
  
  lines ( x
          , y
          , col = "black"
          , lwd = 3
  )
  
  grid()
  
  plotregression(a, b, xlim= c(0, max(x)), ylim = ylim )
  
  lr <- ifelse (b[2] > 0 ,"topleft", "topright")
  

  # Add labels to pronosed dates
  
  dev.off()
  
}
x <- 0:10
y <- c(1081
       ,1246
       ,1232
       ,1370
       ,1575
       ,1747
       ,1809
       ,1932
       ,2065
       ,2250
       ,2485
)
ys <- y[2:11] - y[1:10]
regression_analysis(x,y)

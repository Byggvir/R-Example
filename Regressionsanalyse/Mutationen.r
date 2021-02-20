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

regression_analysis <- function ( 
    x 
    , y 
    , main = "Regressionsanalyse"
    , sub = "") {
  
  ra <- lm(log(y) ~ x)
  ci <- confint(ra,level = CI)
  l <- length(x)
  
  print(ra)
  print(ci)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  

  print(round(exp(b*7),2))
  xlim <- c(min(x),max(x)+10)
  ylim <- c(  0
              , ( max(
                c(   exp(a)
                     , exp(a+b*(max(x)+10))
                )
              ) %/% 100 + 1 ) * 100
  )
  
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
    main = main
    , cex.main = 3
  )
  t <- title ( 
      sub = sub
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
  
  plotregression(a, b, xlim= c(0, max(x)+10), ylim = ylim )
  
  lr <- ifelse (b[2] > 0 ,"topleft", "topright")
  
  legend(
    lr
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = c( 
      paste(round((exp(ci[2,1])-1)*100,2),"%")
      , paste(round((exp(ra$coefficients[2])-1)*100,2),"%")
      , paste(round((exp(ci[2,2])-1)*100,2),"%"))
    , col = c(
      "green"
      , "orange"
      , "red"
    )
    , lty = 3 
    , lwd = 3
    , cex = 2)
   
}

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
       ,2692
       
)

l <- length(y)
x <- 0:(l-1)

ys <- y[2:l] - y[1:(l-1)]

png( paste( "png/RA_Mutationen"
            , ".png"
            , sep = ""
)
, width = 1920
, height = 1080
)

par(  mar = c(10,5,10,5) 
    , bg = "lightyellow"
    , mfcol = c(1,2))

regression_analysis(
  x
  , y
  , "Mutationen Baden-Württemberg"
  , "Kumulierte Fälle ab 08.02.2021" )


regression_analysis(
  0:(l-2)
  , ys
  , "Mutationen Baden-Württemberg"
  , "Täglichen neue Fälle ab 08.02.2021" )

dev.off()

#!/usr/bin/env Rscript

# Regressionsanalyse

MyScriptName <- 'Delta-1'
setwd("~/git/R-Example")

f <- function (y) {
  
  return ( -log(y/(1-y)))
  
}

sigmoid <- function (x,a=0,b=1) {

  return ( 1/(1+exp(a+b*x)))
  
}
plotsigmoid <- function (a, b, col, xlim, ylim) {
  
  par( new=TRUE )
  
  curve(  sigmoid(x,a,b)
          , from=xlim[1]
          , to=xlim[2]
          , lty = 2
          , lwd = 5
          , xlim = xlim
          , ylim = ylim
          , col = col
          , xlab = ""
          , ylab = ""
          , xaxt = "n"
          , yaxt = "n"
          , cex.axis = 2
          , cex.lab = 3
          , cex.main = 5      
  )
  
}

source("lib/copyright.r")
source("lib/myfunctions.r")
source("common/ta_regressionanalysis.r")
source("common/rki_sql.r")

require(data.table)
library(readODS)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)

voc <- read_ods(path='data/VOC_VOI_Tabelle.ods',sheet=1)

sw <- 21

n <- nrow(voc)
s <- rowSums(voc) - voc[,1]

png( paste( "png/"
            , MyScriptName
            , ".png"
            , sep = ""
)
, width = 3840
, height = 2160
)

par(
    mar = c(10,10,10,10) 
  , bg = rgb(0.95,0.95,0.95,1)
  #, mfrow = c(4,3)
)

CI <- 0.95
for ( sw in 14 ){
for ( m in n) {
  
  df <- data.frame(
      x = voc[sw:m,1]
    , y = voc[sw:m,4]/s[sw:m]
  )

  xlim <- c(df$x[1],40)
  ylim <- c(0,1)

  ra <- lm(f(y) ~ x, data=df)
  ci <- confint(ra,level = CI)

  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

  plot(   voc[1:m,1]
        , voc[1:m,4]/s[1:m]
        , type = "l"
        , lwd = 5
        , col = "blue"
        , xlim = xlim
        , ylim = ylim
        , main = ""
        , sub = ""
        , xlab = "Kalenderwoche 2021"
        , ylab = "Anteil"
        , cex.axis = 2
        , cex.lab = 1.5
  )
  title ( main = "Anteil der Delta-Variante in DEU"
        , cex.main = 5
        , cex.sub = 3
  )
  title (   sub = paste("Regression von Kw", sw, "bis", m)
          , cex.sub = 2
          , line = 6
  )
  
  legend( "topleft"
          , title = "Sigmoid 1/(1+e^(a+b*t))"
          , legend = c(
              paste ("a = ", round(ra$coefficients[1],2), " [CI95 ",round(ci[1,1],2)," / ",round(ci[1,2],2),']', sep = '')
            , paste ("b = ", round(ra$coefficients[2],2), " [CI95 ",round(ci[2,1],2)," / ",round(ci[2,2],2),']', sep = '')
          )
          , cex = 2
          , inset = 0.05
  )
  legend( "bottomright"
          , title = "Linien"
          , legend = c(   "Untere Grenze CI 95%"
                        , "Obere Grenze Ci 95%"
                        , "Mittlerer Verlauf"
                        , "Realer Verlauf"
            )
          , col = c("green","red","black","blue")
          , lty = 1
          , lwd = 2
          , cex = 2
          , inset = 0.05
  )

  lines(  
      voc[m:n,1]
    , voc[m:n,4]/s[m:n]
    , type = "l"
    , lty = 3
    , lwd = 5
    , col = "blue"
    )

  plotsigmoid(
      a[3]
    , b[1]
    , col = "green"
    , xlim
    , ylim
    )

  plotsigmoid(
      a[2]
    , b[2]
    , col = "black"
    , xlim
    , ylim
  )

  plotsigmoid(
      a[1]
    , b[3]
    , col = "red"
    , xlim
    , ylim
  )

  kw <- isoweek(Sys.Date())

  abline(
      v = kw
    , lwd = 5
    , lty = 2
    , col = "grey"
  )
  
  text( 
    35
    , 0.5
    , labels = paste ("R-Delta / R-Alpha\n", round(exp(-4/7*b[2]),2), " [CI95% ", round(exp(-4/7*b[3]),2)," - ",round(exp(-4/7*b[1]),2),"]", sep='')
    , cex = 6
  ) 
  
  
  grid()

} # m

} # sw
dev.off()

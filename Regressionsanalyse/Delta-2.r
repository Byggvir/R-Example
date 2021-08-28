#!/usr/bin/env Rscript

# Regressionsanalyse

MyScriptName <- 'Delta-2'
setwd("~/git/R-Example")

f <- function (y) {
  
  return ( -log(y/(1-y)))
  
}

sigmoid <- function (x,a,b) {
  
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

# ---

voc <- read_ods(path='data/VOC_VOI_Tabelle.ods',sheet=1)

kw <- isoweek(Sys.Date())

n <- nrow(voc)
# n <- 25
s <- rowSums(voc) - voc[,1]

png( paste( "png/"
            , MyScriptName
            , "-Sequenzierungen.png"
            , sep = ""
)
, width = 3840
, height = 2160
)

par(  mar = c(10,10,10,10) 
      , bg = rgb(0.95,0.95,0.95,1)
)

pal1 <- rgb(c(0,1,0.5),0,c(1,0,0.5),1)
pal2 <- rgb(c(0,1,0.5),0,c(1,0,0.5),0.1)

#xlim <- limbounds(data[,1])
xlim <- c(voc$KW[1],(kw %/% 5+1)*5)
ylim <- c(0,1)

CI <- 0.95

plot( NA
  , main = "VOC in DE"
  , xlab = "Kalenderwochen 2021"
  , ylab = "Anteil [%]"
  , xlim = xlim
  , ylim = ylim
  , cex.main = 6
  , cex.sub = 4
)

cols <- colnames(voc)

legend( "left"
        , legend = cols[2:3]
        , title = "VOC"
        #, col = c("red","green")
        , col = pal1[1:2]
        , lwd = 2
        , lty = 2
        , inset = 0.1
        , cex = 4
)

title(
      sub="Prozentualer Anteil der VOC Alfa und Delta"
    , cex.sub = 2
    , line = +5
)


for ( i in c(2,4) ) {

  df <- data.frame(
    x = voc[14:n,1]
    , y = voc[14:n,i]/s[14:n]
  )
  
  ra <- lm(f(y) ~ x, data=df)
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

  sigma <- sqrt(voc[,i]*(s-voc[,i])/s)/s

  polygon( c(voc[,1],rev(voc[,1]))
         , c((voc[,i]/s + 1.96*sigma),rev(voc[,i]/s - 1.96*sigma))
         , col = pal2[i-1]
  )
  
  lines(
      voc[,1]
    , voc[,i]/s
    , type = "l"
    , lwd = 5
    , col = pal1[i-1]

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
  , col = "yellow"
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
}

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
  , cex = 4
  , adj = 1
) 


grid()

dev.off()

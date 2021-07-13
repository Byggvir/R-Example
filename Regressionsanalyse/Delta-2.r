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

data <- read_ods(path='data/VOC_VOI_Tabelle.ods',sheet=1)

s <- rowSums(data) - data[,1]

df <- data.frame(
    x = data[14:25,1]
  , y = data[14:25,3]/s[14:25]
)

CI <- 0.95

ra <- lm(f(y) ~ x, data=df)
print(ra)  

ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

print(a[2])
print(b[2])

xlim <- c(df$x[1],40)
ylim <- c(0,1)

png( paste( "png/"
            , MyScriptName
            , ".png"
            , sep = ""
)
, width = 1920
, height = 1080
)
par(  mar = c(10,10,10,10) 
      , bg = rgb(0.95,0.95,0.95,1)
)

plot(   df$x
        , df$y
        , type = "l"
        , lwd = 5
        , col = "blue"
        , xlim = xlim
        , ylim = ylim
        , main = "Anteil der Delta-Variante in DEU"
        , xlab = "Kalenderwoche 2021"
        , ylab = "Anteil"
        , cex.axis = 2
        , cex.lab = 3
        , cex.main = 5
        )

par( new=TRUE )

curve(  sigmoid(x,a[3],b[1])
      , from=xlim[1]
      , to=xlim[2]
      , lty = 2
      , lwd = 5
      , xlim = xlim
      , ylim = ylim
      , col = "green"
      , xlab = ""
      , ylab = ""
      , xaxt = "n"
      , yaxt = "n"
      , cex.axis = 2
      , cex.lab = 3
      , cex.main = 5      
      )

par( new=TRUE )

curve(  sigmoid(x,a[2],b[2])
        , from=xlim[1]
        , to=xlim[2]
        , lty = 2
        , lwd = 5
        , xlim = xlim
        , ylim = ylim
        , col = "black"
        , xlab = ""
        , ylab = ""
        , xaxt = "n"
        , yaxt = "n"
        , cex.axis = 2
        , cex.lab = 3
        , cex.main = 5      
)

par( new=TRUE )

curve(  sigmoid(x,a[1],b[3])
        , from=xlim[1]
        , to=xlim[2]
        , lty = 2
        , lwd = 5
        , xlim = xlim
        , ylim = ylim
        , col = "red"
        , xlab = ""
        , ylab = ""
        , xaxt = "n"
        , yaxt = "n"
        , cex.axis = 2
        , cex.lab = 3
        , cex.main = 5      
)

abline(
  v = 26
  , lwd = 5
  , lty = 2
  , col = "grey"
  )

grid()
dev.off()

png( paste( "png/"
            , MyScriptName
            , "-Sequenzierungen.png"
            , sep = ""
)
, width = 1920
, height = 1080
)
par(  mar = c(10,10,10,10) 
      , bg = rgb(0.95,0.95,0.95,1)
)

pal1 <- rgb(c(0,1),c(1,0),0,1)
pal2 <- rgb(c(0,1),c(1,0),0,0.1)

xlim <- limbounds(data[,1])
ylim <- c(0,100)
plot( NA
  , main = "VOC in DE"
  , xlab = "Kalenderwochen 2021"
  , ylab = "Anteil [%]"
  , xlim = xlim
  , ylim = ylim
  , cex.main = 3
  , cex.sub = 2
)

title(
      sub="Prozentualer Anteil der VOC Alfa und Delta"
    , cex.sub = 2
    , line = +5
)
rgb

for ( i in 2:3) {
  
  sigma <- sqrt(data[,i]*(s-data[,i])/s)/s
  print(1.96*sigma)
  polygon( c(data[,1],rev(data[,1]))
         , c((data[,i]/s + 1.96*sigma),rev(data[,i]/s - 1.96*sigma))*100
         , col = pal2[i-1]
  )
  
  lines(
      data[,1]
    , data[,i]/s*100
    , type = "l"
    , lwd = 3
    , col = pal1[i-1]

    )

}
cols <- colnames(data)

legend( "bottomleft"
       , legend = cols[2:3]
       , pal[1:2]
       , lwd = 2
       , lty = 1
       , inset = 2
       )
grid()

dev.off()

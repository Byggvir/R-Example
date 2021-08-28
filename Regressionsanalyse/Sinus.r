#!/usr/bin/env Rscript

# Regressionsanalyse
#

# y = a + b*sin(x)

MyScriptName <- 'Sinus'
setwd("~/git/R-Example")

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

n <- 50
true_A <- 2
true_B <- 1.5

u <- pi 

xlim <- c(0,u)
ylim <- c(true_A - true_B - 0.5,true_A + true_B + 0.5)

df <- data.frame(
    x = sort(runif(n, min=0, max=u))
)

df$y <- true_A + true_B * sin(df$x) + rnorm(n,0,true_B/20)

CI <- 0.95

ra <- lm(y ~ sin(x), data=df)
print(ra)  

ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

print(a[2])
print(b[2])

png( paste( "png/RASinus"
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
        , main = "Regressionsanalyse einer Sinuskurve"
        , xlab = "x"
        , ylab = "a+b*sin(x)"
        , cex.axis = 2
        , cex.lab = 3
        , cex.main = 5
        )

par( new=TRUE )

curve(  a[1]+b[1]*sin(x)
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

curve(  a[2]+b[2]*sin(x)
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

curve(  a[3]+b[3]*sin(x)
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

grid()
dev.off()

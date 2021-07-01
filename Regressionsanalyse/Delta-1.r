#!/usr/bin/env Rscript

# Regressionsanalyse
MyScriptName <- 'Delta-1'
setwd("~/git/R-Example")

f <- function (y) {
  
  return ( -log(y/(1-y)))
  
}

s <- function (x,a,b) {
  
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
# 
# 15 	 89,0 		 1,9  	0,3  	0,1 
# 16 	 91,1 		 0,9 	 0,4  	0,7 
# 17 	 90,4 		 0,7 	 1,0  	1,4 
# 18 	 87,1 		 2,5  	0,9  	1,8 
# 19 	 89,0 		 1,3  	0,4  	2,6 
# 20 	 91,2 		 0,7 	 0,8  	3,0 
# 21 	 90,9 		 0,6 	 0,8  	3,6 
# 22 	 84,5 		 0,4  	0,9  	7,9 
# 23 	 74,4 		 0,4 	 1,2  	17,0 
# 24 	 54,9 		 0,6 	 1,3  	36,7 

# KW16	4440		34
# KW17	3537		55
# KW18	4279		89
# KW19	3491		101
# KW20	3616		119
# KW21	2574		102
# KW22	1892		177
# KW23	1040		238
# KW24	338		226

df <- data.frame(
    x = 14:24
  , y = c(   0.1
           , 0.1
           , 0.7
           , 1.4
           , 1.8
           , 2.6
           , 3.0
           , 3.6
           , 7.9
           , 17.0
           , 36.7
           )/100
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

curve(  s(x,a[3],b[1])
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

curve(  s(x,a[2],b[2])
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

curve(  s(x,a[1],b[3])
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

abline(v=26, , lwd= 5,  lty=2, col="grey")
grid()
dev.off()


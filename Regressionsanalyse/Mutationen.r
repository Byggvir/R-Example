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
source("common/ta_regressionanalysis.r")
source("common/rki_sql.r")

require(data.table)
library(readODS)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)


CI <- 0.95

f_exp <- function (x,a,b) {
  
  return(exp(a+b*x))
  
}

f_lin <- function (x,a,b) {
  
  return (a+b*x)
  
}

f_exp_1 <- function (x) {
  return(log(x))
}

f_x_1 <- function (x) {
  return(x)
}

regression_analysis <- function ( 
    x 
    , y 
    , main = "Regressionsanalyse"
    , sub = ""
    , is_log = TRUE
    , goback = 0
    ) {
  
  if (is_log == TRUE) {
    f <- f_exp
    f1 <- f_exp_1
  } else {
    f <- f_lin
    f1 <- f_x_1
  }
  
  l <-length(x)
  ra <- lm(f1(y[1:(l-goback)]) ~ x[1:(l-goback)])
#  ra <- lm(f1(y) ~ x)
  
  summary.lm(ra)  
  
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  l <- length(x)
  
  xlim <- c(min(x),max(x))
  ylim <- c(  0
              , ( max(
                c(   f(0,a,b)
                     , f(max(x),a,b)
                     , y
                )
              ) %/% 100 + 1 ) * 100
  )

  plot(  NA # x[1:(l-goback)]
         , NA # y[1:(l-goback)]
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
  
  plotregression(a, b, xlim= c(0, max(x)), ylim = ylim, is_log = is_log)
  
  t <- title ( 
    main = main
    , cex.main = 3
  )
  t <- title ( 
      sub = sub
    , cex.sub = 2
    , line = -3
  )

  lines ( x[1:(l-goback)]
          , y[1:(l-goback)]
          , col = "black"
          , type = "l"
          , lwd = 3
          , lty = 4
  )
  if (goback > 0) {
  lines ( x[(l-goback):l]
          , y[(l-goback):l]
          , col = "grey"
          , lwd = 3
          , lty = 4
  )
  }
  
  grid()


  lr <- ifelse (b[2] > 0 ,"topleft", "bottomright")
  
  legend(
    lr
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = paste(
        round((f(1,a,b)/f(0,a,b)-1)*100,2),"%, R=", 
        round((f(4,0,b)),3)
    )
    , col = c(
      "green"
      , "orange"
      , "red"
    )
    , lty = 3 
    , lwd = 3
    , cex = 2
    )
  
s <- summary(ra)

print(s)
print (sd(y-f(x,a[2],b[2])))



return (ra);

}

daten <- read_ods( path='/home/thomas/git/R-Example/data/Mutation BW.ods' )


l <- length(daten$Tag)

aa <- 1
aa <- l - 28

ee <- l

goback <- 0

png( paste( "png/RA_Mutationen-Tag-"
            , aa
            , '-'
            , ee
            , '-'
            , goback
            , ".png"
            , sep = ""
)
, width = 3840
, height = 2160
)

par(  mar = c(10,5,10,5) 
      , bg = rgb(0.95,0.95,0.95,1)
      , mfcol = c(3,4))

x <- aa:ee
y <- daten[,3]

                  
for ( i in c(3:12,14)) {
  
  ra <- regression_analysis(
    x - aa
    , daten[aa:ee,i]
    , paste("VoC: Baden-Württemberg Alter" ,colnames(daten)[i])
    , paste("Kumulierte Fälle VoC ab 08.02.2021; Tag ",aa," - ",ee)
    , is_log = TRUE
    , goback = goback
    )

  ci <- confint(ra,level = CI)

  print(f_exp(4,0,ra$coefficients[2]))
  print(f_exp(4,0,ci[2,]))

}

amtag <-daten[2:l,14] - daten[1:(l-1),14]

xlim <- c(1,(l-1))
ylim <- limbounds(amtag)

# plot(  1:(l-1)
#        , amtag
#        , main = ""
#        , sub = ""
#        , xlab = "Tag x"
#        , ylab = "Anzahl"
#        , ylim = ylim
#        , type = "l"
#        , lwd = 3
#        , xlim = xlim
#        , col = "black"
#        , cex = 4
#        
# )

ra <- regression_analysis(
  1:(l-1)
  , amtag
  , paste("VoC: Baden-Württemberg Alter" ,colnames(daten)[i])
  , paste("Tägliche Fälle VoC ab 08.02.2021; Tag ",aa," - ",ee)
  , is_log = TRUE
  , goback = goback
)

ci <- confint(ra,level = CI)

print(f_exp(4,0,ra$coefficients[2]))
print(f_exp(4,0,ci[2,]))

grid()

copyright(c("TAr"))


dev.off()

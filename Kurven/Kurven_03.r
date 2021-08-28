#!/usr/bin/env Rscript

# Sigmoid - Exponential

library(tidyverse)

if (rstudioapi::isAvailable()){
  
  # When called in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When called from command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')
setwd(WD)

source("lib/copyright.r")

png("Kurven03.png",width=1920,height=1080)

a <- -5
b <- 5

ylim <- c(0,exp(b)*1.1)
ylim <- c(0,1)

curve(1/(1+exp(-x))
    , a, b
    , ylim = ylim
    , col="green"
    , ylab="y"
    , lwd = 5
    )
title(main = " Exponential-Funktion vs Sigmoid-Funktion"
      , xlab = "Time t"
      , cex.main = 3
      )


grid()

par(new=TRUE)

curve(exp(x)
      , a, b
      , ylim = ylim
      , col="blue"
      , xaxt="n"
      , yaxt="n"
      , xlab=""
      , ylab=""
)

par(new=TRUE)

curve( exp(x) - 1/(1+exp(-x))
       , a, b
       , ylim = ylim
      , col="red"
      , xaxt="n"
      , yaxt="n"
      , xlab=""
      , ylab=""
      , lwd = 3
)

par(new=TRUE)

curve( exp(-exp(-x))
       , a, b
       , ylim = ylim
       , col="black"
       , xaxt="n"
       , yaxt="n"
       , xlab=""
       , ylab=""
       , lwd = 5
)
legend(
      "topleft"
    , legend = c(
          's(t) = 1/(1-exp(-t))'
        , 'f(t) = exp(t)'
        , 'd(t) = f(t)-s(t)'
                 )
    , col = c(
          'green'
        , 'blue'
        , 'red'
        )
    , lty = 1
    , cex = 3
    , inset = 0.1
    )

text( b
     , exp(b)-1/(1+exp(-b))
     , labels = paste(
         round ( exp(b)-1/(1+exp(-b) ), 5)
         , '\n ~ '
         , round( (exp(b)-1/(1+exp(-b))) / exp(b) * 100,1)
        ,'%'
        , sep =''
     )
     , pos = 2
     , cex = 3
     , col = 'red'
)

copyright(holders = c('TAr'))

dev.off()

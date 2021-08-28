#!/usr/bin/env Rscript

require(data.table)
library(tidyverse)

# Set Working directory to git root

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

png( "Kurven08.png"
     , width = 1920
     , height = 1080)

a <- 0
b <- 100

s <- c(4096,32)

mycolors <- c("red","green","blue")

R_A <- 1.4

r <- c(log(R_A/2)/7,log(R_A)/7)
print (r)

curve(s[1]*exp(r[1]*x)
      , a , b
      , col = mycolors[1]
      , xlab=""
      , ylab=""
      , ylim = c(0,5000)
      , lwd = 3
)

par(new = TRUE)

curve(s[2]*exp(r[2]*x)
      , a , b
      , col = mycolors[2]
      , xaxt="n"
      , yaxt="n"
      , xlab=""
      , ylab=""
      , ylim = c(0,5000)
      , lwd = 3
    )

par(new = TRUE)

curve(s[1]*exp(r[1]*x) + s[2]*exp(r[2]*x)
      , a , b
      , col = mycolors[3]
      , xaxt="n"
      , yaxt="n"
      , xlab=""
      , ylab=""
      , ylim = c(0,5000)
      , lwd = 3
)
title (
  main = 'Verdrängung alte durch neue Variante'
  , cex.main = 4
  , sub = "Verdrängung"
)
legend(
  "topleft"
  , title = 'Variante'
  , legend = c('Alte', 'Neue', "Summe")
  , col = mycolors
  , lty = c(1,1)
  , lwd = 5
  , cex = 2
  , inset = 0.1
)

grid()

copyright(holders = c('TAr'))

dev.off()


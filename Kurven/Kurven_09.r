#!/usr/bin/env Rscript

# Set Working directory to git root

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

png("png/Kurven_09.png",width=1000,height=1000)

a <- log(c(100,150))
b <- log(c(1.1,1.25))

xlim <- c(0,4)
ylim <- c(0,4000)

col <- c("red","green")

plot( NA
      , xlab = "x"
      , ylab="y"
      , xlim = xlim
      , ylim = ylim
      , main = "Infizierte"
      , yaxt = 'n'
      )

for ( i in 1:2) {
    par (new = TRUE)
    curve(exp(a[i]+ b[i]*x)
        , 0 , 14
        , col = col[i]
        , xlab=""
        , ylab=""
        , xlim = xlim
        , ylim = ylim
        , axes = FALSE
    )
    axis( side=2)

}

col <- c('black','blue')

for ( i in 1:2) {
    par (new = TRUE)
    curve((exp(a[i]+ b[i]*x)-exp(a[i]))/b[1]
          , 0 , 14
          , col = col[i]
          , xlab = ""
          , ylab = ""
          , xlim = xlim
          , ylim = ylim
          , axes = FALSE
    )
    axis( side=2)
    
}
dev.off()

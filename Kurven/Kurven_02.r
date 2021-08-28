#!/usr/bin/env Rscript

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

png("png/Kurven.png",width=1000,height=1000)

par(mfcol=c(2,2))

curve(sin(x)
    , 0 , 2*pi
    , col="red"
    , ylab="y"
    , main="sin(x)"
    )

curve(cos(x)
    , 0 , 2*pi
    , col="blue"
    , xlab="x"
    , ylab="y"
    , main="cos(x)"
    )

curve(tan(x)
    , 0 , pi/2
    , col="blue"
    , xlab="x"
    , ylab="y"
    , ylim=c(0,10)
    , main="tan(x)"
    )

curve(1/tan(x)
    , 0 , pi/2
    , col="blue"
    , xlab="x"
    , ylab="y"
    , ylim=c(0,10)
    , main="cotan(x)"
    )

dev.off()

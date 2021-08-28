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

png("png/Kurven.png",width=1000,height=1000)

curve(sin(x)
    , 0 , 2*pi
    , col="red"
    , ylab="y"
    )

par(new=TRUE)

curve(cos(x)
    , 0 , 2*pi
    , col="blue"
    , xaxt="n"
    , yaxt="n"
    , xlab=""
    , ylab=""
    )

dev.off()

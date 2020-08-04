#!/usr/bin/env Rscript

png("Kurven.png",width=1000,height=1000)

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

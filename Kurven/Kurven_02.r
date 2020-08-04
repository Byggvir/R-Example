#!/usr/bin/env Rscript

png("Kurven.png",width=1000,height=1000)

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

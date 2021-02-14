#!/usr/bin/env Rscript

setwd("~/git/R-Example")

source("lib/copyright.r")

png( "Kurven04.png"
     , width = 1920
     , height = 1080)

a <- -5
b <- 5

s <- function (x) {

  return (1/(1+exp(-x)))

}

ylim <- c(0,1)

curve( s(x)
    #  1/(1+exp(-x))
    , a, b
    , ylim = ylim
    , col="green"
    , ylab="y"
    , lwd = 5
    )

grid()

par(new=TRUE)

curve( s(x)*(1-s(x))
       # curve(1/(1+exp(-x))
       , a, b
       , ylim = ylim
       , col="green"
       , ylab="y"
       , lwd = 5
       , lty = 2
)

title(main = "Sigmoid - Gompertz-Funktion"
      , xlab = "Time t"
      , cex.main = 3
      )

par(new=TRUE)

curve( exp(-exp(-x))
       , a, b
       , ylim = ylim
       , col="red"
       , xaxt="n"
       , yaxt="n"
       , xlab=""
       , ylab=""
       , lwd = 5
)

par(new=TRUE)

curve( exp(-exp(-x)-x)
       , a, b
       , ylim = ylim
       , col="red"
       , xaxt="n"
       , yaxt="n"
       , xlab=""
       , ylab=""
       , lwd = 5
       , lty = 2
)

legend(
      "topleft"
    , legend = c(
          's(t) = 1/(1-exp(-t))'
        , 's´(t)'
        , 'g(t) = exp(-exp(-t))'
        , 'g´(t)'
                 )
    , col = c(
          'green'
          , 'green'
          , 'red'
          , 'red'
        )
    , lty = c(1,2,1,2)
    , lwd = 5
    , cex = 3
    , inset = 0.1
    )

# text( b
#      , exp(b)-1/(1+exp(-b))
#      , labels = paste(
#          round ( exp(b)-1/(1+exp(-b) ), 5)
#          , '\n ~ '
#          , round( (exp(b)-1/(1+exp(-b))) / exp(b) * 100,1)
#         ,'%'
#         , sep =''
#      )
#      , pos = 2
#      , cex = 3
#      , col = 'red'
# )


copyright(holders = c('TAr'))

dev.off()

print( c(s(0),s(0)*(1-s(0)) ))

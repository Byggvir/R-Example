#!/usr/bin/env Rscript
#
#
# Script: SEIR.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"SEIR"


# Epidemiologie SEIR Modell

library(data.table)

options( 
  digits = 7
  , scipen = 999
  , Outdec = "."
  , max.print = 3000
)

setwd("~/git/R-Example")

pEI <- 1/4 # probability to get infectious after being exposed
pIR <- 1/10 # probability to recover when infectious

Tn <- 200 # Number of iterations (days)

Population <- 83200000 # Population at start

R0 <- 3 * pIR # Reproduction value at start (persons / day)

png(filename="png/SEIR.png"
  , width = 1920
  , height = 1080
  )


SEIR <- data.table(
    S = c( Population - 100 , rep(0,Tn-1))
  , E = c( 100 ,rep(0,Tn-1))
  , I = rep( 0 , Tn )
  , R = rep( 0 , Tn )
)

for  (i in 2:Tn) {
  
  SEIR$R[i] <- SEIR$R[i-1] + SEIR$I[i-1] * pIR
  
  SEIR$I[i] <- SEIR$I[i-1] * ( 1 - pIR ) + SEIR$E[i-1] * pEI 
  
  Infect <-  R0 * SEIR$S[i-1] / Population * SEIR$I[i-1]
  
  if (Infect > SEIR$S[i-1] ) { Infect = SEIR$S[i-1] }
  
  SEIR$E[i] <- SEIR$E[i-1] * ( 1 - pEI ) + Infect
  
  SEIR$S[i] <- SEIR$S[i-1] - Infect
  
}

plot( 1:Tn
     , SEIR$S
     , type="l"
     , lwd = 2
     , col = "blue"
     , xlab = "Tag"
     , ylab = "Anzahl"
     , xlim = c( 50, Tn)
     , ylim = c(0, Population)
     , main = "SEIR Modell"
)
grid()

par ( new = TRUE )

plot( 1:Tn 
     , SEIR$E
     , type="l"
     , lwd = 2
     , col = "orange"
     , xlab = ""
     , xlim = c( 50, Tn)
     , ylab = ""
     , ylim = c(0, Population)
)

par ( new = TRUE )

plot( 1:Tn 
     , SEIR$I
     , type="l"
     , lwd = 2
     , col = "red"
     , xlab = ""
     , xlim = c( 50, Tn)
     , ylab = ""
     , ylim = c(0, Population)
)

par ( new = TRUE )

plot(1:Tn 
     ,  SEIR$R
     , type="l"
     , lwd = 2
     , col = "green"
     , xlab = ""
     , xlim = c( 50, Tn)
     , ylab = ""
     , ylim = c(0, Population)
)

legend( "right"
      , legend = c("Suceptible", "Exposed", "Infectuos", "Recovered")
#      , lty = 2
      , col = c("blue","orange","red","green")
      , cex = 3
      , lwd = 2
      , bg = "lightgray"
      )
print (SEIR)

dev.off()

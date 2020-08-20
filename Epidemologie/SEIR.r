#!/usr/bin/env Rscript
library(data.table)

Tn <- 20  # Anzahl Iterationen

Start <- 1000

R0 <- 3 # Reproduntionszahl R zum Zeitpunkt 0

options ( digits = 4 )

SEIR <- data.table(
    S = c(Start,rep(0,Tn-1))
  , E = c(1,rep(0,Tn-1))
  , I = c(1,rep(0,Tn-1))
  , R = rep(0,Tn)
)

for  (i in 2:Tn) {

SEIR$R[i] <- SEIR$R[i-1] + SEIR$I[i-1]
SEIR$I[i] <- SEIR$E[i-1]
SEIR$E[i] <- R0*SEIR$E[i-1]*SEIR$S[i-1]/SEIR$S[1]
SEIR$S[i] <- SEIR$S[i-1] - SEIR$E[i]

}
print(SEIR)

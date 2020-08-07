#!/usr/bin/env Rscript

# Load statistics from RKI test report
# Source https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-08-05-de.pdf?__blob=publicationFile
#
# 

tests <- read.csv("../data/rki_testungen.csv")
cases <- read.csv("../data/Germany.csv")

l <- length(cases$Kw)

cases$incCases <- c(0, cases$Cases[2:l]-cases$Cases[1:l-1])
cases$incDeath <- c(0, cases$Deaths[2:l]-cases$Deaths[1:l-1])

tests$proportion <- tests$Positiv / tests$Testungen 

# Assumed test sensitivity and spezifity

sens <- 0.999
spez <- 0.998

fp <- 1 - spez
fn <- 1 - sens

tests$prevalence <- ( tests$Positiv - tests$Testungen * fp ) / ( tests$Testungen * sens - tests$Testungen * fp )
tests$TP <- tests$Testungen * tests$prevalence * sens
tests$TN <- tests$Testungen * ( 1 - tests$prevalence ) * sens
tests$FP <- tests$Testungen * ( 1 - tests$prevalence ) * fp
tests$FN <- tests$Testungen * tests$prevalence * fn
tests$Infected <- tests$TP + tests$FN
tests$PPP <- tests$TP / tests$Positiv

Kw <- unique(tests$Kw)

for ( i in Kw ) {
  tests$New[tests$Kw ==i] <- sum(cases$incCases[cases$Kw==i])
}

options(scipen=10)

png("rki_prevalence.png",width=1920,height=1080)

par(mar=c(5.1, 10, 4.1, 10),las=1)

par(mfcol=c(1,2))

plot( tests$Kw
    , tests$prevalence * 100
    , ylim=c(0,10)
    , type="b"
    , lwd=2
    , col="green"
    , xlab="Calendar weeks"
    , ylab=""
    , yaxt="n"
    , main="Estimated Prevalence"
    , sub=paste("Sensitivity ", sens*100, "%; Specificity ", spez*100,"%")
    )
    
lines(tests$Kw
    , tests$proportion * 100
    , type="b"
    , lwd=2
    , col="blue"
    )

legend (
    "topright"
    , legend=c("Prevalence","Proportion of positive", "Positive predictive power")
    , col=c(
          "green"
        , "blue"
        , "orange")
    , lty=1
    )

axis( side=2
    , col="blue" 
    , tick=TRUE,    
    , col.axis="blue"
)
title(ylab="Prevalence [%]", col.lab="green", las=0)
title(ylab="Proportion positive tested [%]", line=1, col.lab="blue", las=0)

grid()

par(new=TRUE)

plot( tests$Kw
    , tests$PPP * 100
    , type="b"
    , lwd=2
    , col="orange"
    , ylim=c(0,100)
    , xaxt="n"
    , yaxt="n"
    , xlab=""
    , ylab=""
    )
    
axis( side=4
    , col="orange"
    , col.axis="orange"
    , tick=TRUE
)
mtext ( "Positive Predictive Power (PPP) [%]"
    , side=4
    , col="orange"
    , line=3,
    , las=0
    )


plot( tests$Kw
    , tests$Infected
    , ylim=c(0,40000)
    , type="b"
    , col="green"
    , xlab="Calendar weeks"
    , ylab=""
    , lwd=3
    , main="Estimated infected"
    , sub=paste("Sensitivity ", sens*100,"%; Specificity ", spez*100,"%")
    )
lines(tests$Kw
    , tests$Positiv
    , type="b"
    , col="blue"
    )

lines(tests$Kw
    , tests$FP
    , type="b"
    , col="red"
    )
lines(tests$Kw
    , tests$New
    , type="b"
    , col="black"
    )

legend (
    "topright"
    , legend=c("Infected","Positive","False positive", "Tested", "New infected")
    , col=c("green","blue","red","orange","black")
    , lty=1
    )
grid()

title(ylab="Count", line=5)

par(new=TRUE)

plot( tests$Kw
    , tests$Testungen
    , ylim=c(0,800000)
    , type="b"
    , lwd=3
    , col="orange"
    , xaxt="n"
    , yaxt="n"
    , xlab=""
    , ylab=""
    )

axis( side=4
    , col.axis="orange"
    , tick=TRUE
    )
mtext ( "Number of tests"
    , col="orange"
    , side=4
    , line=5,
    , las=0
    )

dev.off()

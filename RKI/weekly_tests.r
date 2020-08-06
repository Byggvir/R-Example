#!/usr/bin/env Rscript

# Load statistics from RKI test report
# Source https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-08-05-de.pdf?__blob=publicationFile
#
# 

data <- read.csv("../data/rki_testungen.csv")
data$proportion <- data$Positiv / data$Testungen 

# Assumed test sensitivity and spezifity

sens <- 0.999
spez <- 0.997

fp <- 1 - spez
fn <- 1 - sens

data$prevalence <- ( data$Positiv - data$Testungen * fp ) / ( data$Testungen * sens - data$Testungen * fp )
data$TP <- data$Testungen * data$prevalence * sens
data$TN <- data$Testungen * ( 1 - data$prevalence ) * sens
data$FP <- data$Testungen * ( 1 - data$prevalence ) * fp
data$FN <- data$Testungen * data$prevalence * fn
data$Infected <- data$TP + data$FN
data$PPP <- data$TP / data$Positiv

options(scipen=10)

png("rki_prevalence",width=1920,height=1080)

par(mar=c(5.1, 10, 4.1, 10),las=1)

par(mfcol=c(1,2))

plot( data$KW
    , data$prevalence * 100
    , ylim=c(0,10)
    , type="b"
    , lwd=2
    , col="green"
    , xlab="Calendar weeks"
    , ylab=""
    , yaxt="n"
    , main="Estimated Prevalence"
    , sub="Sensitivity 99.9%; Specificity 99.7%"
    )
    
lines(data$KW
    , data$proportion * 100
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

plot( data$KW
    , data$PPP * 100
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


plot( data$KW
    , data$Infected
    , ylim=c(0,40000)
    , type="b"
    , col="green"
    , xlab="Calendar weeks"
    , ylab=""
    , lwd=3
    , main="Estimated infected"
    , sub="Sensitivity 99.9%; Specificity 99.7%"
    )
lines(data$KW
    , data$Positiv
    , type="b"
    , col="blue"
    )

lines(data$KW
    , data$FP
    , type="b"
    , col="red"
    )

legend (
    "topright"
    , legend=c("Infected","Positive","False positive", "Tested")
    , col=c("green","blue","red","orange")
    , lty=1
    )
grid()

title(ylab="Count", line=5)

par(new=TRUE)

plot( data$KW
    , data$Testungen
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

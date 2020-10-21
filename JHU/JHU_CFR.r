#!/usr/bin/env Rscript

library(data.table)
require("readODS")

setwd("~/git/R-Example")
source("lib/copyright.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


CFR <- read_ods("data/CFR.ods",sheet=6)

png("png/CFR_US.png", width = 1920, height = 1080)

AgeGroups <- CFR[,"AgeGroup"]
                 
par( mfcol=c(3,2), 
    mar=c(10,10,10,10))

options(digits = 3,OutDec = ",")

sCases <- sum(CFR$Cases)


bp <- barplot( CFR$Deaths/CFR$Cases *100
               , col = "red"
               , xlab="Altersgruppe"
               , ylab="[%]"
               , ylim=c(0,40)
               , main="Fallsterblichkeit (CFR) COVID-19 in US"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$Deaths/CFR$Cases *100
      , labels = paste(round(CFR$Deaths/CFR$Cases *100,digits = 2),"%")
      , pos = 3
      , cex = 2
      , col = "black"
      
)

bp <- barplot( CFR$Cases
               , col = "orange"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,max(CFR$Cases)*2)
               , main="Fälle COVID-19 in US"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$Cases
      , labels = paste(CFR$Cases, "\n(", round(CFR$Cases/sCases*100,1), "%)", sep="")
      , pos = 3
      , cex = 2
      , col = "black"
      
)

sDeaths <- sum(CFR$Deaths)

bp <- barplot( CFR$Deaths
               , col = "red"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,max(CFR$Deaths)*2)
               , main="Gestorbene COVID-19 in US"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$Deaths
      , labels = paste(CFR$Deaths, "\n(", round(CFR$Deaths/sDeaths*100,1), "%)", sep="")
      , pos = 3
      , cex = 2
      , col = "black"
      
)

sPeople <- sum(CFR$People)

bp <- barplot( CFR$People / sPeople * 100000
               , col = "blue"
               , xlab="Altersgruppe"
               , ylab="Bevölkerung / 100000"
               , ylim=c(0,max(CFR$People / sPeople * 100000)*1.5)
               , main=paste("Bevölkerung US 2018 (",round(sPeople/1e6,2),"Mio.)") 
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$People / sPeople * 100000
      , labels = paste(round(CFR$People/ sPeople * 100000,2), "\n(", round(CFR$People/sPeople*100,1), "%)", sep="")
      , pos = 3
      , cex = 2
      , col = "black"
      
)

bp <- barplot( CFR$Cases/CFR$People * 100000
               , col = "orange"
               , xlab="Altersgruppe"
               , ylab="Anzahl / 100000"
               , ylim=c(0,max(CFR$Cases/CFR$People * 100000)*1.2)
               , main="Fälle / Bevölkerung COVID-19 in US"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$Cases/CFR$People *100000
      , labels = paste(round(CFR$Cases/CFR$People * 100000,digits = 2))
      , pos = 3
      , cex = 2
      , col = "black"
      
)

bp <- barplot( CFR$Deaths/CFR$People * 100000
               , col = "red"
               , xlab="Altersgruppe"
               , ylab="Anzahl / 100000"
               , ylim=c(0,max(CFR$Deaths/CFR$People * 100000)*1.2)
               , main="Gestorbene / Bevölkerung COVID-19 in US"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$Deaths/CFR$People *100000
      , labels = paste(round(CFR$Deaths/CFR$People * 100000,digits = 2))
      , pos = 3
      , cex = 2
      , col = "black"
      
)

title(sub=paste("Quelle: CDC, Altersverteilung Covid-19 vom ", heute, "\nTotal popolation data.census.gov 2018", sep = "" ) , line = 5)
copyright(c("CDC","TAr"))
grid()
dev.off()

#!/usr/bin/env Rscript

require("readODS")

setwd("~/git/R-Example")

CFR <- read_ods("data/SterbeFälleAlter.ods",sheet=2)

reported <- "19.08.2020"

png("png/CFR_DEU.png", width = 1920, height = 1080)

par(mar=c(10,10,10,10))

options(digits = 3,OutDec = ",")

bp <- barplot( (CFR$Male+CFR$Female)/CFR$Cases *100
    , col = "red"
    , xlab="Altersgruppe"
    , ylab="Sterblichkeit [%]"
    , ylim=c(0,40)
    , main="Fallsterblichkeit COVID-19 in Deutschland"
    , cex.axis = 2
    , cex.names = 2
    , cex.main = 4
    , cex.lab = 3
    , names.arg = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")
    )

text( x = bp 
    , y = (CFR$Male+CFR$Female)/CFR$Cases *100
    , labels = paste(round((CFR$Male+CFR$Female)/CFR$Cases *100,digits = 2),"%")
    , pos = 3
    , cex = 2
    , col = "red"
    
)

title(sub=paste("Quelle RKI, Altersverteilung 33. Kw und Fallzahlen vom",reported),line = 5)

grid()
dev.off()

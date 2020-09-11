#!/usr/bin/env Rscript

require("readODS")

setwd("~/git/R-Example")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

AgeGroups <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")

CFR <- read_ods("data/SterbeFälleAlter.ods",sheet=2)
Cases <- read_ods("data/SterbeFälleAlter.ods",sheet=3)

Kw <- max (Cases$Kw) 
    
png( "png/CFR_DEU.png"
     , width = 1920
     , height = 1080
     )

par( mfcol=c(3,1), 
     mar=c(10,10,10,10))
options(digits = 3,OutDec = ",")

sCases <- sum(CFR$Cases)

bp <- barplot( CFR$Cases
               , col = "red"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,max(CFR$Cases)*1.2)
               , main="Fälle in Deutschland"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)

text( x = bp 
      , y = CFR$Cases
      , labels = paste(CFR$Cases, " (", round(CFR$Cases/sCases*100,1), "%)", sep="")
      , pos = 3
      , cex = 2
      , col = "red"
      
)


sDeaths <- sum(CFR$Male) + sum(CFR$Female)

bp <- barplot( (CFR$Male+CFR$Female)
               , col = "red"
               , xlab="Altersgruppe"
               , ylab="Anzahl"
               , ylim=c(0,max(CFR$Male+CFR$Female)*1.2)
               , main="Todesfälle in Deutschland"
               , cex.axis = 2
               , cex.names = 2
               , cex.main = 4
               , cex.lab = 3
               , names.arg = AgeGroups
)


text( x = bp 
      , y = (CFR$Male+CFR$Female)
      , labels = paste(CFR$Male+CFR$Female, " (", round((CFR$Male+CFR$Female)/sDeaths*100,1), "%)", sep="")
      , pos = 3
      , cex = 2
      , col = "red"
      
)

bp <- barplot( (CFR$Male+CFR$Female)/CFR$Cases * 100
    , col = "red"
    , xlab="Altersgruppe"
    , ylab="[%]"
    , ylim=c(0,40)
    , main="Fallsterblichkeit COVID-19 in Deutschland"
    , cex.axis = 2
    , cex.names = 2
    , cex.main = 4
    , cex.lab = 3
    , names.arg = AgeGroups
    )


text( x = bp 
    , y = (CFR$Male+CFR$Female)/CFR$Cases * 100 + 2
    , labels = paste(round((CFR$Male+CFR$Female)/CFR$Cases * 100,1),"%", sep="")
    , cex = 2
    , col = "red"
    
)

title(sub=paste("Quelle RKI, Altersverteilung ", Kw , ". Kw und Todesfälle vom ", heute, sep = "" ) , line = 5)

grid()
dev.off()

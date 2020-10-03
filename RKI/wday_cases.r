#!/usr/bin/env Rscript

#
# Auswertung der Fallzahlen und Tote nach den Meldungen der John Hopkins University
# Quelle: https://github.com/CSSEGISandData/COVID-19
#
require(data.table)
library(REST)

setwd("~/git/R-Example")
source("common/rki_download.r")

country <- "DEU"

cases <- get_rki_kumtab()

png(paste("png/Wochentag-",country,".png",sep=""),width=1920,height=1080)

ShortDayNames <- c(
      "Mo"
    , "Di"
    , "Mi"
    , "Do"
    , "Fr"
    , "Sa"
    , "So"
    )

l <- length(cases$Date)

cases$UpDown <- c(0, sign(cases$incCases[2:l]-cases$incCases[1:(l-1)]))

print(head(cases))

UpDown <- table(cases$WTag[cases$Kw>9],cases$UpDown[cases$Kw>9])

colnames(UpDown, do.NULL = FALSE)

if (length(colnames(UpDown))==2) {
  colnames(UpDown) <- c("Down","Up")
} else {  
  colnames(UpDown) <- c("Down","Equal","Up")
}

WTag <- aggregate(cbind(incCases,incDeaths)~WTag, FUN=sum, data=cases)

KorrekturFaktor <- data.table (
  WTag = 0:6
  , Faktor = sum(WTag$incCases)/WTag$incCases/7
)

print(KorrekturFaktor)

par( mfcol = c(1,2) )

options( digits = 3 )

plot( 0:6
    , UpDown[1:7,"Down"]
    , type="b"
    , col=c("green")
    , ylim=c(0,40)
    , xaxt="n"
    , xlab="Wochentag"
    , ylab="Anzahl"
    , main=paste("Änderungen gegenüber Vortag (", country,")")
    , sub="Tageswert höher / niedirger als Vortag, seit Kalenderwoche 10"
    )
    
lines( 0:6
    , UpDown[1:7,"Up"]
    , type="b"
    , col=c("red")
    )

if (length(colnames(UpDown))==3) {
lines( 0:6
       , UpDown[1:7,"Equal"]
       , type="b"
       , col=c("blue")
)
}

axis( 1
    , at= 0:6
    , labels=ShortDayNames
    )

legend( "topright"
    , legend=c("Höher als Vortag", "Wie Vortag", "Niedriger als Vortag")
    , col=c("red", "blue", "green")
    , lty=1
    )
grid()

plot( 
      0:6
    , WTag$incCases/sum(WTag$incCases)*100
    , type="b"
    , col="blue"
    , xaxt="n"
    , ylim= c(5,25)
    , xlab="Wochentag"
    , ylab="Fälle pro Tag [%]"
    , main=paste("Erkrankungen und Todesfälle (",country,")")
    , sub="Verteilt auf Wochentage; Quelle: RKI Kumulative Fälle"
    )
    
lines( 
      0:6
    , WTag$incDeaths/sum(WTag$incDeaths)*100
    , type="b"
    , col="black"
    )

abline( h = 100/7
  , col = "black"
  , lty = 3)

text( 6,14, labels=100/7)

axis( 1
    , at= 0:6
    , labels=ShortDayNames
    )
legend( "topright"
    , legend=c("Erkrankte","Gestorbene")
    , col=c("blue", "black")
    , lty=1
    )

grid()

dev.off()
#!/usr/bin/env Rscript
#
#
# Script: RKI_wday_cases.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_wday_cases"


#
# Auswertung der Fallzahlen und Tote nach den Meldungen des Robert-Koch-Institutes
#

require(data.table)
library(REST)

setwd("~/git/R-Example")
#source("common/rki_download.r")
source("common/rki_sql.r")
source("lib/copyright.r")

tendenz <- function (x) {
  
  r <- rep(0,length(x))
           
  for (i in 1:length(x)) {
    if (x[i] > 0) r[i] <- 1
    else if (x[i] < 0) r[i] <- -1
    
  }
  return (r)
  
}

country <- "DEU"

cases <- sqlGetRKI()

png(  paste("png/RKI_Wochentag-",country,".png",sep="")
    , width = 1920
    , height = 1080
    )

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

# cases$UpDown <- c(0, sign(cases$incCases[2:l]-cases$incCases[1:(l-1)]))

cases$UpDown <- c(0, tendenz(cases$incCases[2:l]-cases$incCases[1:(l-1)]))

UpDown <- table(cases$WTag[cases$Kw>9],cases$UpDown[cases$Kw>9])

colnames(UpDown, do.NULL = FALSE)

if (length(colnames(UpDown))==2) {
  
  colnames(UpDown) <- c("Down","Up")

  } else {  
  
  colnames(UpDown) <- c("Down","Equal","Up")

  }

WTag <- aggregate(cbind(incCases,incDeaths)~WTag, FUN=sum, data=cases)

colnames(WTag) <- c("WTag","incCases","incDeaths") 
  
KorrekturFaktor <- data.table (
  WTag = 0:6
  , Faktor = sum(WTag$incCases)/WTag$incCases/7
)

print(KorrekturFaktor)

par( 
    mar = c(10,6,10,6)
  , mfcol = c(1,2) )

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

copyright()

WTagCases <- WTag$incCases/sum(WTag$incCases) * 100
WTagDeaths <- WTag$incDeaths/sum(WTag$incDeaths) * 100

plot( 
      0:6
    , WTagCases
    , type="b"
    , col="blue"
    , xaxt="n"
    , ylim= c(0,25)
    , xlab="Wochentag"
    , ylab="Fälle pro Tag [%]"
    , main=paste("Neuinfektionen und Todesfälle (",country,")")
    , sub="Meldedatum RKI nach Wochentag"
    )

text(0:6
     , WTagCases - 0.5
     , paste(round(WTagCases,2),"%",sep="")
     , adj = 0.5
     , cex = 1.5
     , col="blue"
)    

lines( 
      0:6
    , WTagDeaths
    , type="b"
    , col="black"
    )

text(0:6
     , WTagDeaths + 0.5
     , paste(round(WTagDeaths,2),"%",sep="")
     , adj = 0.5
     , cex = 1.5
     , col = "black"
)  

abline( h = 100/7
  , col = "black"
  , lty = 3)

text( 6, 14
      , labels = paste(round(100/7,2),"%", sep="")
      )

axis( 1
    , at= 0:6
    , labels=ShortDayNames
    )

legend( "topright"
    , legend=c("Neuinfektionen","Gestorbene")
    , col=c("blue", "black")
    , lty=1
    )

grid()

dev.off()

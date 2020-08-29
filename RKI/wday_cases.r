#!/usr/bin/env Rscript

#
# Auswertung der Fallzahlen und Tote nach den Meldungen der John Hopkins University
# Quelle: https://github.com/CSSEGISandData/COVID-19
#

args = commandArgs(trailingOnly=TRUE)

#if (length(args)==2) {
#  inputfile <- args[1]
#  country <- args[2]
#} else {
#    stop("Two arguments must be supplied (input file, contry).", call.=FALSE)
#}

inputfile <- "data/Germany.csv"
country <- "DEU"

cases <- read.csv(inputfile)

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

l <- length(cases$Kw)

cases$incCases <- c(0, cases$Cases[2:l]-cases$Cases[1:l-1])
cases$incDeaths <- c(0, cases$Deaths[2:l]-cases$Deaths[1:l-1])

cases$UpDown <- c(0, sign(cases$incCases[2:l]-cases$incCases[1:l-1]))

UpDown <- table(cases$WTag[cases$Kw>9],cases$UpDown[cases$Kw>9])

colnames(UpDown, do.NULL = FALSE)

if (length(colnames(UpDown))==2) {
  colnames(UpDown) <- c("Down","Up")
} else {  
  colnames(UpDown) <- c("Down","Equal","Up")
}

par(mfcol=c(1,2))
options(digits=3)

plot( 0:6
    , UpDown[1:7,"Down"]
    , type="b"
    , col=c("green")
    , ylim=c(0,22)
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

axis(1
    , at= 0:6
    , labels=ShortDayNames
    )

legend( "topright"
    , legend=c("Höher als Vortag", "Wie Vortag", "Niedriger als Vortag")
    , col=c("red", "blue", "green")
    , lty=1
    )
grid()

WTag <- aggregate(cbind(incCases,incDeaths)~WTag, FUN=sum, data=cases)

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
    , sub="Verteilt auf Wochentage; Quelle: https://github.com/CSSEGISandData/COVID-19"
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

axis(1
    , at= 0:6
    , labels=ShortDayNames
    )
legend( "topright"
    , legend=c("Erkrankte","Gestorbene")
    , col=c("blue", "black")
    , lty=1
    )

grid()

#print(WTag$incCases/sum(WTag$incCases))
#print(WTag$incDeaths/sum(WTag$incDeaths))

dev.off()

